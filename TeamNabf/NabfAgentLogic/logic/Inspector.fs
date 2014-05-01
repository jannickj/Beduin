namespace NabfAgentLogic
module Inspector =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants

    let distanceToOccupyJobMod = 0.1

    ///////////////////////////////////Helper functions//////////////////////////////////////
    let calculateDesireOccupyJob (j:Job) (s:State) = 
        let ((_,newValue,_,_),(jobData)) = j      
        let oldJobValue = 
                            if (s.MyJobs.IsEmpty) then
                                0
                            else
                                (getJobValueFromJoblist s.MyJobs s)

        let jobTargetNode = 
            match jobData with
            | OccupyJob (_,zone) -> zone.Head
        

        let (distanceToJob,personalValueMod) = (getDistanceToJobAndNumberOfEnemyNodes jobTargetNode s)
        

        int <| (((float newValue) * personalValueMod) - (float oldJobValue))    +     (-(distanceToJob * DISTANCE_TO_OCCUPY_JOB_MOD))    +    INSPECTOR_OCCUPYJOB_MOD
   

    ////////////////////////////////////////Logic////////////////////////////////////////////

    
    let spontanousInspectAgent (s:State) = 
        let uninspectedNearbyEnemies = List.filter (fun a -> a.Role.IsNone) (nearbyEnemies s s.Self)
        match uninspectedNearbyEnemies with
        | [] -> None
        | head::tail ->     
            Some(
                    "inspect agent " + head.Name
                    , Activity
                    , [Requirement(agentHasFulfilledRequirement head.Name (fun ag -> ag.Role.IsSome))]
                )

    let applyToOccupyJob (s:State) = 
        let applicationList = createApplicationList s JobType.OccupyJob calculateDesireOccupyJob
        Some(
                "apply to all occupy jobs"
                , Communication
                , [Plan(fun state -> applicationList)]
            )
    
    let doOccupyJob (s:State) = None    
    
    let applyToDisruptJob (s:State) = None //advanced feature
    
    let doDisruptJob (s:State) = None //advanced feature
    
    let findAgentToInspect (s:State) = 
        Some(
                "find and inspect an agent"
                , Activity
                , [Requirement(
                    fun state ->  
                        match s.LastAction with
                        | (Inspect _) -> true
                        | _ -> false
                )]
            )