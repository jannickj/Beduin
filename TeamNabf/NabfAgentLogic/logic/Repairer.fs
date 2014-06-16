namespace NabfAgentLogic
module Repairer =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants

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
        int <| (((float newValue) * personalValueMod) - (float oldJobValue))    +     (-((float distanceToJob) * DISTANCE_TO_REPAIR_JOB_MOD))    +    REPAIRER_REPAIRJOB_MOD
   

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let spontanouslyRepairNearbyDamagedAgent (inputState:State) = 
        //let nearbyDamagedAgent = List.filter (fun a -> (float a.Health.Value) < ((float a.MaxHealth.Value) * SPONTANOUS_REPAIR_PERCENTAGE)) (nearbyAllies inputState)
        let nearbyDamagedAgent = List.filter (fun a -> a.Status = Disabled) (nearbyAllies inputState)
        
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
                    , [Requirement (Repaired agent)]
                    )
        | [] -> None