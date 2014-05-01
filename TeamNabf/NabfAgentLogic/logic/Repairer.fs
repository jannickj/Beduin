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
        

        let (distanceToJob,personalValueMod) = (getDistanceToJobAndNumberOfEnemyNodes jobTargetNode s)
        

        int <| (((float newValue) * personalValueMod) - (float oldJobValue))    +     (-(distanceToJob * DISTANCE_TO_REPAIR_JOB_MOD))    +    REPAIRER_REPAIRJOB_MOD
   

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let spontanouslyRepairNearbyDamagedAgent (s:State) = 
        let nearbyDamagedAgent = List.filter (fun a -> (float a.Health.Value) < ((float a.MaxHealth.Value) * SPONTANOUS_REPAIR_PERCENTAGE)) (nearbyAllies s)
        match nearbyDamagedAgent with
        | [] -> None
        | head::tail ->     
            Some(
                    "repair agent " + head.Name
                    , Activity
                    , [Requirement(agentHasFulfilledRequirement head.Name (fun ag -> ag.Health = ag.MaxHealth))]
                )

    let applyToRepairJob (s:State) = 
        let applicationList = createApplicationList s JobType.RepairJob calculateDesireRepairJob
        Some(
                "apply to all repair jobs"
                , Communication
                , [Plan(fun state -> applicationList)]
            )
    
    let doRepairJob (s:State) = None