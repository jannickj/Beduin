namespace NabfAgentLogic
module Repairer =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants
    open GeneralLib

    ///////////////////////////////////Helper functions//////////////////////////////////////
    let calculateDesireRepairJob (j:Job) (s:State) = 
        let ((_,newValue,_,_),(jobData)) = j      
        let oldJobValue = 
            if (s.MyJobs.IsEmpty) then
                0
            else
                (getJobValueFromJoblist s.MyJobs s)

        let jobTargetNode = 
            match jobData with
            | RepairJob (node,_) -> node
        

        let distanceToJob = (distanceBetweenAgentAndNode jobTargetNode s)
        
        let personalValueMod = 1 |> float//if an agent has some kind of "personal" preference 
                                         //that modifies how much it desires the new job, using the input modifier 
                 

        //final desire
        int <| (((((float newValue) * personalValueMod) - (float oldJobValue))   +    (-((float distanceToJob) * DISTANCE_TO_REPAIR_JOB_MOD))   ) )


    ////////////////////////////////////////Logic////////////////////////////////////////////

    let giveMyLocationToRepairee (inputState:State) =
        match inputState.MyJobs with
        | (curJobId,_)::_ -> 
            match getJobFromJobID inputState curJobId with
            | (_,RepairJob (_, agentName)) -> 
                normalIntention ( "send my location to "+agentName, 
                                  Communication, 
                                  [Plan (fun s -> 
                                        Some [Communicate <| SendMail (s.Self.Name,agentName,MyLocation s.Self.Node)]
                                   )]
                                )
                |> Some
            | _ -> None    
        | _ -> None


    let spontanouslyRepairDamagedAgent (inputState:State) = 
        let nearbyDamagedAgent = List.filter (fun a -> a.Status = Disabled) (alliesHere inputState inputState.Self.Node)
        match nearbyDamagedAgent with
        | [] -> None
        | head::tail ->     
            Some <| normalIntention (
                    "repair agent " + head.Name
                    , Activity
                    , [Requirement (Repaired head.Name)]
                )

    let applyToRepairJob (inputState:State) = 
        let applicationList = createApplicationList inputState JobType.RepairJob calculateDesireRepairJob
        let isMyRepairJob msg = 
            match msg with 
            | Communicate act ->
                match act with
                | ApplyJob (id,_) -> 
                    match getJobFromJobID inputState id with
                    | (_,RepairJob(_,an)) -> an = inputState.Self.Name
                    | _ -> false
                | _ -> false
            | _ -> false

        let applicationListWithoutSelf = List.filter (fun msg -> not <| isMyRepairJob msg) applicationList
        Some <| normalIntention (
                "apply to all repair jobs"
                , Communication
                , [Plan (fun state -> Some applicationListWithoutSelf)]
            )
    
    let workOnRepairJob (inputState:State) = 
        let myJobs = List.map (fun (id,_) -> getJobFromJobID inputState id) inputState.MyJobs
        let myRepairJobs = getJobsByType JobType.RepairJob myJobs
        match myRepairJobs with
        | ((Some id,_,_,_),RepairJob(_,agentName))::_ -> 
            let node = match tryFindAgentByName agentName inputState.FriendlyData with
                       | Some agent -> agent.Node
                       | None -> "unknown" 
            
            normalIntention 
                ( "repair agent " + agentName + " on node " + node
                , Activity
                , [ Plan (fun s -> Some [Communicate <| SendMail (s.Self.Name,agentName,GoingToRepairYou)]);
                    Requirement (Repaired agentName)]
                )
            |> Some
        | _ -> None