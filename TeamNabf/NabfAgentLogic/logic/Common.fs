namespace NabfAgentLogic
module Common =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants
    open Graphing.Graph
    open Logging
    


    ///////////////////////////////////Helper functions//////////////////////////////////////

    //Calculate value of repairing of an agenttype
    let calculateRepairValue agentType =
        match agentType with
        | Saboteur -> SABOTEUR_REPAIR_PRIORITY
        | Repairer -> REPAIRER_REPAIR_PRIORITY 
        | Sentinel -> SENTINEL_REPAIR_PRIORITY
        | Explorer -> EXPLORER_REPAIR_PRIORITY
        | Inspector -> INSPECTOR_REPAIR_PRIORITY
       

    //Calculate the desire to an occupy job
    let calculateDesireOccupyJob  modifier (j:Job) (s:State)= 
        let ((_,newValue,_,_),(jobData)) = j      
        let oldJobValue = 
                            if (s.MyJobs.IsEmpty) then
                                0
                            else
                                (getJobValueFromJoblist s.MyJobs s)

        let jobTargetNode = 
            match jobData with
            | OccupyJob (_,zone) -> zone.Head

        let distanceToJob = (distanceBetweenAgentAndNode jobTargetNode s)
        
        let personalValueMod = 1 |> float//if an agent has some kind of "personal" preference 
                                         //that modifies how much it desires the new job, using the input modifier 
        
        let isEnabled =
            if (s.Self.Status = EntityStatus.Disabled) then
                0.0
            else
                1.0

        //final desire
        int <| (((((float newValue) * personalValueMod) - (float oldJobValue))   +    (-((float distanceToJob) * DISTANCE_TO_OCCUPY_JOB_MOD))   +    modifier) * isEnabled)
    //Try to find any repair jobs for a specific agent
    let tryFindRepairJobForAgent (agentName) (jobs:Job list) =
        List.tryFind (fun (_,job) -> 
                            match job with
                            | RepairJob (_,a) -> agentName = a
                            | _ -> false) jobs



    let nodeHasMinValue (state:State) node =
        let n = state.World.[node] 
        if (n.Value.IsSome) then
            n.Value.Value >= MINIMUM_VALUE_VALUE
        else
            false

    ////////////////////////////////////////Logic////////////////////////////////////////////

  

    let giveMyLocationToMyRepairer (inputState:State) =
        match (inputState.Self.Status,Map.tryFind MyRepairer inputState.Relations) with
        | (Normal, Some repairer) -> 
            Some <| normalIntention 
                    ( "send my location to repairer "+repairer,
                      Communication,
                      [
                        Plan (fun s -> 
                            [Communicate <| SendMail (s.Self.Name,repairer,MyLocation s.Self.Node)]
                            |> Some)
                      ]
                    )
        | _ -> None 

    //Try to make it so the agent has explored one more node
    let exploreMap (inputState:State) = 
        let otherAgentsOnMyNode = List.filter (fun a -> a.Node = inputState.Self.Node && not(a.Name = inputState.Self.Name)) inputState.FriendlyData

        let tryNearestUnexplored = nearestVertexSatisfying inputState isUnexplored
        match tryNearestUnexplored with
        | Some nearestUnexplored ->
            let goal = 
                if (nodeHasNoOtherFriendlyAgentsOnIt inputState inputState.Self.Node) then
                    Explored nearestUnexplored
                else
                    if (myRankIsGreatest inputState.Self.Name otherAgentsOnMyNode) then
                        let nextBest = findNextBestUnexplored inputState
                        match nextBest with
                        | Some vertex -> Explored vertex
                        | None -> Explored nearestUnexplored
                    else
                        Explored nearestUnexplored

            Some <| normalIntention 
                 ( "explore one more node."
                 , Activity
                 , [Requirement goal]
                 )
        | _ -> None

    //Posts or removes a repair job on the master server for repairing itself
    let postRepairJob (inputState:State) =
        match tryFindRepairJobForAgent inputState.Self.Name inputState.Jobs,inputState.Self.Status with
            | None,Disabled ->
                let myNode = inputState.Self.Node
                let myName = inputState.Self.Name
                let jobPriority = calculateRepairValue inputState.Self.Role.Value
                let numberOfAgentsNeeded = 1
                let communicateJob = CreateJob ( (None,jobPriority,JobType.RepairJob,numberOfAgentsNeeded),RepairJob(myNode,myName))
                normalIntention 
                    ( "post repair job for me."
                    , Communication
                    , [ Plan <| fun _ -> Some [ Communicate <| communicateJob ]]
                    )
                |> Some
            | Some ((Some id,_,_,_),_),Normal ->
                normalIntention 
                    ( "remove repair job for me."
                    , Communication
                    , [ Plan <| fun _ -> Some [ Communicate <| RemoveJob id ]]
                    )
                |> Some
            | _ -> None    

    //When disabled, tries to locate repairer if None found continue with normal duty
    let getRepaired (inputState:State) = 
        match Map.tryFind MyRepairer inputState.Relations,inputState.Self.Status with
        | Some an,Disabled ->
            normalIntention 
                (   "get repaired by "+an,
                    Activity,
                    [   Requirement (GetCloseTo an)
                    ;   Plan (fun s -> if s.Self.Status = Disabled then Some [Perform(Recharge)] else None)
                    ]
                )
            |> Some
        | _ -> None

            
    //Find a node of at least value 8 to stand on.
    let generateSomeValue (inputState:State) = 
        Some  <| normalIntention ( "get some(8) value"
             , Activity
             , [ Requirement <| AtMinValueNode SOME_VALUE_VALUE
               ; Plan <| fun _ -> Some ([Perform Recharge]) 
               ]
             )

    //Find a node of at least value 6 to stand on.
    let generateLittleValue (inputState:State) = 
        Some  <| normalIntention ( "get little(6) value"
             , Activity
             , [ Requirement <| AtMinValueNode LITTLE_VALUE_VALUE
               ; Plan <| fun _ -> Some ([Perform Recharge]) 
               ]
             )

    //Find a node of at least value 4 to stand on.
    let generateLeastValue (inputState:State) = 
        Some  <| normalIntention ( "get least(4) value"
             , Activity
             , [ Requirement <| AtMinValueNode LEAST_VALUE_VALUE
               ; Plan <| fun _ -> Some ([Perform Recharge]) 
               ]
             )

    //Find a node of at least value 2 to stand on.
    let generateMinimumValue (inputState:State) = 
        Some <| normalIntention  ( "get minimum(2) value"
             , Activity
             , [ Requirement <| AtMinValueNode MINIMUM_VALUE_VALUE
               ; Plan <| fun _ -> Some ([Perform Recharge]) 
               ]
             )

//    let _oldKnowledge = ref (Set.empty<Percept>)
//    let _redundant = ref 0
//    let lockObject = new System.Object()

    let shareKnowledge (inputState:State) : Option<Intention> =
//         lock lockObject (fun () -> let ss = !_oldKnowledge
//                                    let ns = List.fold (fun ps p -> 
//                                                
//                                                Set.add p ps) ss s.NewKnowledge
//                                    ()
//                                    )               
            Some<| normalIntention 
                    (   "share my knowledge", 
                        Communication, 
                        [Plan   ( fun state -> 
                                    if (state.NewKnowledge.Length > 0) then
                                        Some [(Communicate <| ShareKnowledge ( state.NewKnowledge))] 
                                    else
                                        None
                                )
                        ]
                    )
         
    
    
    let applyToOccupyJob  modifier (inputState:State) = 
        let applicationList = createApplicationList inputState JobType.OccupyJob (calculateDesireOccupyJob modifier)
        Some <| normalIntention ( 
                "apply to all occupy jobs"
                 , Communication
                 , [Plan (fun state -> Some applicationList)]
             )
    

    let workOnOccupyJob (inputState:State) =
        logStateImportant inputState Intentions <| sprintf  "my jobs are: %A" (List.map fst inputState.MyJobs)//delete this line. no longer needed
        let myJobs = List.map (fun (id,_) -> getJobFromJobID inputState id) inputState.MyJobs
        let myOccupyJobs = getJobsByType JobType.OccupyJob myJobs
        match myOccupyJobs with
        | ((id,_,_,_),_)::_ -> 
            let (_,node) = List.find (fun (jid,_) -> id.Value = jid) inputState.MyJobs
            Some <| normalIntention 
                ( "occupy node " + node
                 , Activity
                 , [ Requirement (At node)
                   ; Plan <| fun _ -> Some [Perform Recharge]
                   ]
                 )
        | [] -> None


    let postAttackJob (inputState:State) = 
        let agentsICanSee = List.filter (fun a -> a.Node <> "") inputState.EnemyData
        let agentsOnValuableNode = List.filter 
                                            (fun a -> 
                                                if (inputState.World.[a.Node].Value.IsSome) then
                                                    inputState.World.[a.Node].Value >= Some NODE_VALUE_TO_ORDER_ATTACK
                                                else
                                                    false
                                            ) 
            
                                            agentsICanSee

        let shouldPostJob = agentsOnValuableNode.Length > 0
        if (shouldPostJob) then
            let node = agentsOnValuableNode.Head.Node
            let nodeValue = inputState.World.[node].Value.Value
            Some <| normalIntention 
                    ( "post attack job on node " + node
                     , Activity
                     , [ Plan <| fun state -> Some [Communicate( CreateJob( (None,nodeValue,JobType.AttackJob,1),AttackJob([node]) ))]]
                     )
        else
            None