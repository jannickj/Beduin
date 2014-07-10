namespace NabfAgentLogic
module Repairer =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants
    open GeneralLib
    open Common

    ///////////////////////////////////Helper functions//////////////////////////////////////

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let giveMyLocationToRepairee (inputState:State) =
        match inputState.MyJobs with
        | (curJobId,_)::_ -> 
            match getJobFromJobID inputState.Jobs curJobId with
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
        let disabledAgentsHere = List.filter (fun a -> a.Status = Disabled) (alliesHere inputState inputState.Self.Node)
        let myTarget = selectBasedOnRank inputState disabledAgentsHere
        match myTarget with
        | None -> None
        | Some agent ->
            Some <| normalIntention (
                    "repair agent " + agent.Name
                    , Activity
                    , [Plan (fun _ -> Some [Perform <| Repair agent.Name])]
                )

    let applyToRepairJob (inputState:State) = 
        if List.length inputState.MyJobs > 0 then
            None
        else
            let desireCalc = (calculateJobDesire JOB_IMPORTANCE_MODIFIER_REPAIR DISTANCE_TO_REPAIR_JOB_MOD 0.0)
            let applicationList = createApplicationList inputState JobType.RepairJob desireCalc
            let isMyRepairJob msg = 
                match msg with 
                | Communicate act ->
                    match act with
                    | ApplyJob (id,_) -> 
                        match getJobFromJobID inputState.Jobs id with
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
        //logStateImportant inputState Logging.Intentions <| sprintf "friendly data: %A" inputState.FriendlyData
        let myJobs = List.map (fun (id,_) -> getJobFromJobID inputState.Jobs id) inputState.MyJobs
        let myRepairJobs = getJobsByType JobType.RepairJob myJobs
        match myRepairJobs with
        | ((Some id,_,_,_),RepairJob(_,agentName))::_ -> 
            let node = match tryFindAgentByName agentName inputState.FriendlyData with
                       | Some agent -> agent.Node
                       | None -> failwith "Can't see agent to repair" 
            
            normalIntention 
                ( "repair agent " + agentName + " on node " + node
                , Activity
                , [ Plan (fun s -> Some [Communicate <| SendMail (s.Self.Name,agentName,GoingToRepairYou)]);
                    Requirement (Repaired agentName)
                  ]
                )
            |> Some
        | _ -> None
