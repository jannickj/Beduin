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
            | (_,RepairJob (_, an)) -> 
                normalIntention ( "send my location to "+an, 
                                  Communication, 
                                  [Plan (fun s -> 
                                        [Communicate <| SendMail (s.Self.Name,an,MyLocation s.Self.Node)]
                                        |> Some
                                   )]
                                )
                |> Some
            | _ -> None    
        | _ -> None


    let spontanouslyRepairDamagedAgent (inputState:State) = 
        let nearbyDamagedAgent = List.filter (fun a -> a.Status = Disabled) (alliesHere inputState inputState.Self.Node)
        logStateImportant inputState Logging.Intentions <| sprintf "nearby damaged agents: %A" nearbyDamagedAgent
        logStateImportant inputState Logging.Intentions <| sprintf "friendly data: %A" inputState.FriendlyData
        match nearbyDamagedAgent with
        | [] -> None
        | head::tail ->     
            Some <| normalIntention (
                    "repair agent " + head.Name
                    , Activity
                    , [Requirement (Repaired head.Name)]
                )

    let applyToRepairJob (inputState:State) = 
        let applicationListWithSelf = createApplicationList inputState JobType.RepairJob calculateDesireRepairJob
        let isAppOfItSelf msg = 
            match msg with 
            | Communicate act ->
                match act with
                | ApplyJob (id,_) -> 
                    match getJobFromJobID inputState id with
                    | (_,RepairJob(_,an)) -> an = inputState.Self.Name
                    | _ -> false
                | _ -> false
            | _ -> false

        let applicationList = List.filter (fun msg -> not <| isAppOfItSelf msg) applicationListWithSelf
        Some <| normalIntention (
                "apply to all repair jobs"
                , Communication
                , [Plan (fun state -> Some applicationList)]
            )
    
    let workOnRepairJob (inputState:State) = 
        let myJobs = List.map (fun (id,_) -> getJobFromJobID inputState id) inputState.MyJobs
        let myRepairJobs = getJobsByType JobType.RepairJob myJobs
        match myRepairJobs with
        | ((id,_,_,_),_)::_ -> 
            let (jobid,node) = List.find (fun (jid,_) -> id.Value = jid) inputState.MyJobs
            let (_,RepairJob(_,agent)) = (getJobFromJobID inputState jobid) : Job
            Some <| normalIntention 
                    ( "repair agent " + agent + " on node " + node
                    , Activity
                    , [ Plan (fun s -> Some [Communicate <| SendMail (s.Self.Name,agent,GoingToRepairYou)]);
                        Requirement (Repaired agent)]
                    )
        | [] -> None