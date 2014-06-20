namespace NabfAgentLogic
module Common =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants
    open Graphing.Graph
    open Logging
    open GeneralLib

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
        int <| (( (JOB_IMPORTANCE_MODIFIER_OCCUPY*(((float newValue) * personalValueMod) - (float oldJobValue)))   +    (-((float distanceToJob) * DISTANCE_TO_OCCUPY_JOB_MOD))   +    modifier) * isEnabled)
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
            
    let exploringNotDone (s:State) = (float s.World.Count) < ( EXPLORE_FACTOR_DONE_EXPLORING * (float s.TotalNodeCount) )

    let notSurveyedEnough (s:State) = s.SimulationStep < SURVEY_MY_NODE_UNTIL_THIS_TURN_IF_NEEDED

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let unapplyFromJobsWhenDisabled (inputState:State) = 
        if inputState.MyJobs.Length = 0 || inputState.Self.Status = EntityStatus.Normal then
            None
        else
            let (jobid,_) = inputState.MyJobs.Head
            Some <| normalIntention 
                        ( "unapply from my job with id " + jobid.ToString() + " because im disabled"
                            , Communication
                            , [ Plan <| fun state -> Some [Communicate( UnapplyJob(jobid) )]]
                            )


    //Try to make it so the agent has explored one more node
    let exploreMap (inputState:State) = 
        let othersOnMyNode = List.filter (fun a -> a.Node = inputState.Self.Node && not(a.Name = inputState.Self.Name)) inputState.FriendlyData
        let otherAgentNames = List.map getAgentName othersOnMyNode
        let tryNearestUnexplored = nearestVertexSatisfying inputState isUnexplored
        match tryNearestUnexplored with
        | Some nearestUnexplored ->
            let goal = 
                if (nodeHasNoOtherFriendlyAgentsOnIt inputState inputState.Self.Node) then
                    Explored nearestUnexplored
                else
                    if (myRankIsGreatest inputState.Self.Name otherAgentNames) then
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
        match tryFindRepairJobForAgent inputState.Self.Name inputState.Jobs, inputState.Self.Status with
            | None, Disabled ->
                let myNode = inputState.Self.Node
                let myName = inputState.Self.Name
                let jobPriority = calculateRepairValue inputState.Self.Role.Value
                let numberOfAgentsNeeded = 1
                let communicateJob = CreateJob ( (None,jobPriority,JobType.RepairJob,numberOfAgentsNeeded),RepairJob(myNode,myName))
                normalIntention 
                    ( "post a repair job for me."
                    , Communication
                    , [ Plan <| fun _ -> Some [ Communicate <| communicateJob ]]
                    )
                |> Some
            | Some ((Some id,_,_,_),_),Normal ->
                normalIntention 
                    ( "remove the repair job for me."
                    , Communication
                    , [ Plan <| fun _ -> Some [ Communicate <| RemoveJob id ]]
                    )
                |> Some
            | _ -> None    

    //When disabled, tries to locate repairer if None found continue with normal duty
    let getRepaired (inputState:State) =
        let repairJob =
            match inputState.MyJobs with
            | (id,_)::_ -> Some (snd <| getJobFromJobID inputState id)
            | _ -> None

        match Map.tryFind MyRepairer inputState.Relations, inputState.Self.Status, repairJob with
        | Some aName, Disabled, Some (RepairJob(_,rName)) when aName = rName && myRankIsGreatest inputState.Self.Name [rName] -> 
            None
        | Some aName,Disabled,_ ->
            normalIntention 
                (   "get repaired by "+aName,
                    Activity,
                    [   Requirement (GetCloseTo aName)
                    ;   Plan (fun s -> if s.Self.Status = Disabled then Some [Perform(Recharge)] else None)
                    ]
                )
            |> Some
        | _ -> None

    let giveMyLocationToMyRepairer (inputState:State) =
        match (inputState.Self.Status, Map.tryFind MyRepairer inputState.Relations) with
        | (Disabled, Some repairer) -> 
            normalIntention 
                ( "send my location to repairer "+repairer,
                  Communication,
                  [
                    Plan (fun s -> Some [Communicate <| SendMail (s.Self.Name,repairer,MyLocation s.Self.Node)])
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
        logStateInfo inputState Intentions <| sprintf  "my jobs are: %A" (List.map fst inputState.MyJobs)
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
        let agentsICanSee = List.filter (fun a -> a.Node <> "" && a.Status = EntityStatus.Normal) inputState.EnemyData
        let agentsOnValuableNode = List.filter 
                                            (fun a -> 
                                                inputState.World.[a.Node].Value.IsSome
                                                && 
                                                inputState.World.[a.Node].Value >= Some MIN_NODE_VALUE_TO_POST_ATTACK
                                            )             
                                            agentsICanSee

        let valuableEnemyControlledNodes = Set.filter (fun n -> 
                                                            inputState.World.[n].Value.IsSome
                                                            && 
                                                            inputState.World.[n].Value >= Some MIN_NODE_VALUE_TO_POST_ATTACK
                                                       ) 
                                                       inputState.NodesControlledByEnemy

        let agentTargetExists = agentsOnValuableNode.Length > 0 
        let nodeTargetExists = valuableEnemyControlledNodes.Count > 0
        if (agentTargetExists || nodeTargetExists) then            
            let node = 
                if (agentTargetExists) then
                    (List.maxBy (fun a -> inputState.World.[a.Node].Value) agentsOnValuableNode).Node
                else
                    List.maxBy (fun vertexName -> inputState.World.[vertexName].Value) (Set.toList valuableEnemyControlledNodes)

            let isAttackJobOnNode onNode job =
                match job with
                | (_,AttackJob(attackNode::_,_)) -> attackNode = onNode
                | _ -> false
                
                
            let jobValue = inputState.World.[node].Value.Value * ATTACK_IMPORTANCE_MODIFIER
            match List.tryFind (isAttackJobOnNode node) inputState.Jobs with
            | Some ((id,_,_,_),_) ->
                Some <| normalIntention 
                        ( "update attack job on node " + node
                        , Communication
                        , [ Plan <| fun state -> Some [Communicate( UpdateJob((id,jobValue,JobType.AttackJob,1),AttackJob([node],inputState.SimulationStep) ))]]
                        )
            | None ->
                Some <| normalIntention 
                        ( "post attack job on node " + node
                         , Communication
                         , [ Plan <| fun state -> Some [Communicate( CreateJob( (None,jobValue,JobType.AttackJob,1),AttackJob([node],inputState.SimulationStep) ))]]
                         )
        else
            None


    let postDefenseJob (inputState:State) = 
        //checking if the agent has an occupy job and that an non-disabled enemy is standing on it's node
        let agentsOnMyNodeWhileImOnOccupyJob =
            match inputState.MyJobs with
            | (jobid,_)::tail -> 
                match (getJobFromJobID inputState jobid) with
                | (_,OccupyJob(_,zoneList)) -> 
                    match List.filter (fun a -> (List.exists (fun n -> n = a.Node) zoneList) && a.Status = EntityStatus.Normal) inputState.EnemyData with
                    | h::t -> Some (h::t)
                    | _ -> None
                | _ -> None
            | _ -> None
            
        if (agentsOnMyNodeWhileImOnOccupyJob.IsNone) then
            None
        else 
            if (agentsOnMyNodeWhileImOnOccupyJob.Value.Head.Role.IsSome 
            && agentsOnMyNodeWhileImOnOccupyJob.Value.Head.Role.Value = AgentRole.Sentinel 
            && agentsOnMyNodeWhileImOnOccupyJob.Value.Head.RoleCertainty >= 50) then
                None
            else
                match inputState.MyJobs.Head with
                | (_,vertex) when vertex <> inputState.Self.Node-> None //if we have not arrived at the job yet, dont post defense
                | _ -> 
                    let node = agentsOnMyNodeWhileImOnOccupyJob.Value.Head.Node
                    let jobValue = inputState.World.[node].Value.Value * DEFENSE_IMPORTANCE_MODIFIER
                    let isAttackJobOnNode onNode job =
                        match job with
                        | (_,AttackJob(attackNode::_,_)) -> attackNode = onNode
                        | _ -> false
                

                    match List.tryFind (isAttackJobOnNode node) inputState.Jobs with
                    | Some ((id,_,_,_),_) ->
                        Some <| normalIntention 
                                ( "update defense job on node " + node
                                , Communication
                                , [ Plan <| fun state -> Some [Communicate( UpdateJob((id,jobValue,JobType.AttackJob,1),AttackJob([node],inputState.SimulationStep) ))]]
                                )
                    | None ->
                        Some <| normalIntention 
                                ( "post defense job on node " + node
                                , Communication
                                , [ Plan <| fun state -> Some [Communicate( CreateJob( (None,jobValue,JobType.AttackJob,1),AttackJob([node],inputState.SimulationStep) ))]]
                                )


