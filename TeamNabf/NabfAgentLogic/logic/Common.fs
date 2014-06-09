namespace NabfAgentLogic
module Common =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants
    open Graphing.Graph
    


    ///////////////////////////////////Helper functions//////////////////////////////////////
    
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

        let distanceToJob = (getDistanceToJobAndNumberOfEnemyNodes jobTargetNode s)
        
        let personalValueMod = 1 |> float//if an agent has some kind of "personal" preference 
                                         //that modifies how much it desires the new job, using the input modifier 
        
        //final desire
        int <| (((float newValue) * personalValueMod) - (float oldJobValue))    +     (-(distanceToJob * DISTANCE_TO_OCCUPY_JOB_MOD))    +    modifier


    //Try to find any repair jobs put up by the agent itself.
    let rec tryFindRepairJob (inputState:State) (knownJobs:Job list) =
            match knownJobs with
            | (_ , rdata) :: tail -> if rdata = RepairJob(inputState.Self.Node,inputState.Self.Name) then Some knownJobs.Head else tryFindRepairJob inputState tail
            | [] -> None


    let nodeHasMinValue (state:State) node =
        let n = state.World.[node] 
        if (n.Value.IsSome) then
            n.Value.Value >= MINIMUM_VALUE_VALUE
        else
            false

    ////////////////////////////////////////Logic////////////////////////////////////////////

    //An agent always wants to have exactly one goal
    let onlyOneJob (inputState:State) = 
        Some(
                "have at most 1 job"
                , Communication
                , [Plan(fun state -> 
                                        match state.MyJobs with
                                        | [] -> Some []
                                        | _ :: tail -> 
                                            Some <| List.map (fun (id,_) -> Communicate (UnapplyJob id)) tail                                       
                        )
                  ]
            )

    //Try to make it so the agent has explored one more node
    let exploreMap (inputState:State) = 
        let agentsOnMyNode = List.filter (fun a -> a.Node = inputState.Self.Node && not(a.Name = inputState.Self.Name)) inputState.FriendlyData
        let goal = 
            if (agentsOnMyNode.IsEmpty) then
                Explored None
    //            findAndDo inputState.Self.Node nodeIsUnexplored CheckGoal "mark as explored" false inputState
            else
                if (myRankIsGreatest inputState.Self.Name agentsOnMyNode) then
                    let nextBest = findNextBestUnexplored inputState
                    match nextBest with
                    | Some vertex -> Explored <| Some vertex
                    | None -> Explored None
    //                findAndDo inputState.Self.Node nodeIsUnexplored CheckGoal "mark as explored" true inputState
                else
                    Explored None

        Some ( "explore one more node."
             , Activity
             , [Requirement goal]
             )

    //When disabled, post a repair job, then recharge while waiting for a repairer. Temporary version to be updated later.
    //Works by creating a plan to recharge one turn each turn.
    let getRepaired (inputState:State) = 
        if inputState.Self.Status = Disabled 
        then
            let j = tryFindRepairJob inputState inputState.Jobs
            let myName = inputState.Self.Name
            match j with
            //I already created a job:
            | Some(_,RepairJob(_,myName)) -> 
                Some("wait for a repairer.",Activity,[Plan (fun s -> Some [Perform(Recharge)])])
            //Otherwise, create the job, then start waiting
            | _ -> 
                let here = inputState.Self.Node
                let communicateJob state = 
                    Some [ Communicate <| CreateJob ( (None,5,JobType.RepairJob,1),RepairJob(state.Self.Node,state.Self.Name) ) ]
                Some ( "get repaired."
                     , Activity
                     , [ Plan <| communicateJob
                       ; Requirement <| Charged None
                       ]
                     )
        else
            None
            
    //Find a node of at leas value 8 to stand on.
    let generateMinimumValue (inputState:State) = 
        Some ( "get minimum value"
             , Activity
             , [ Requirement Occupied ]
             )

//    let _oldKnowledge = ref (Set.empty<Percept>)
//    let _redundant = ref 0
//    let lockObject = new System.Object()

    let shareKnowledge (s:State) : Option<Intention> =
//         lock lockObject (fun () -> let ss = !_oldKnowledge
//                                    let ns = List.fold (fun ps p -> 
//                                                
//                                                Set.add p ps) ss s.NewKnowledge
//                                    ()
//                                    )               
            Some ("share my knowledge", Communication, [Plan (fun state -> 
                                                                            if (state.NewKnowledge.Length > 0) then
                                                                                Some [(Communicate <| ShareKnowledge ( state.NewKnowledge))] 
                                                                            else
                                                                                None
                                                                                )])
         
    
    
    let applyToOccupyJob  modifier (inputState:State) = 
        let applicationList = createApplicationList inputState JobType.OccupyJob (calculateDesireOccupyJob modifier)
        Some ( "apply to all occupy jobs"
             , Communication
             , [Plan (fun state -> Some applicationList)]
             )
    

    let workOnOccupyJob (inputState:State) =
        let myJobs = List.map (fun (id,_) -> getJobFromJobID inputState id) inputState.MyJobs
        let myOccupyJobs = getJobsByType JobType.OccupyJob myJobs
        match myOccupyJobs with
        | ((id,_,_,_),_)::_ -> 
            let (_,node) = List.find (fun (jid,_) -> id.Value = jid) inputState.MyJobs
            Some ( "occupy node " + node
                 , Activity
                 , [ Requirement (At node)
                   ; Plan <| fun _ -> Some [Perform Recharge]
                   ]
                 )
        | [] -> None