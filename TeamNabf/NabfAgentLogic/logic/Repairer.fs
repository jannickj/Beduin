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
        
        //final desire
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
                    , [Requirement(fun state -> agentHasFulfilledRequirementFriendlies head.Name state (fun ag -> ag.Health = ag.MaxHealth))]
                )

    let applyToRepairJob (s:State) = 
        let applicationList = createApplicationList s JobType.RepairJob calculateDesireRepairJob
        Some(
                "apply to all repair jobs"
                , Communication
                , [Plan(fun state -> applicationList)]
            )
    
    let workOnRepairJob (s:State) = 
        let myJobs = List.map (fun (id,_) -> getJobFromJobID s id) s.MyJobs
        let myRepairJobs = getJobsByType JobType.RepairJob myJobs
        match myRepairJobs with
        | ((id,_,_,_),_)::_ -> 
            let (jobid,node) = List.find (fun (jid,_) -> id.Value = jid) s.MyJobs
            let (_,RepairJob(_,agent)) = (getJobFromJobID s jobid) : Job
            Some
                (   "repair agent " + agent + " on node " + node
                ,   Activity
                ,   [
                        Requirement(fun s -> s.Self.Node = node)
                        ; Requirement(
                                    fun state ->  
                                        match s.LastAction with
                                        | (Repair _) -> true
                                        | _ -> false
                )]
                )
        | [] -> None