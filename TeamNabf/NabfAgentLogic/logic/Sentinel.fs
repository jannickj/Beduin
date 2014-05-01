namespace NabfAgentLogic
module Sentinel =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants

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
        

        int <| (((float newValue) * personalValueMod) - (float oldJobValue))    +     (-(distanceToJob * DISTANCE_TO_OCCUPY_JOB_MOD))    +    SENTINEL_OCCUPYJOB_MOD
   

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let applyToOccupyJob (s:State) = 
        let applicationList = createApplicationList s JobType.OccupyJob calculateDesireOccupyJob
        Some(
                "apply to all occupy jobs"
                , Communication
                , [Plan(fun state -> applicationList)]
            )
    
    let doOccupyJobThenParryIfEnemiesClose (s:State) = None             
    
    let applyToDisruptJob (s:State) = None //advanced feature
    
    let doDisruptJobThenParryIfEnemiesClose (s:State) = None //advanced feature