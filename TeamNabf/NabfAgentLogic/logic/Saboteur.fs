namespace NabfAgentLogic
module Saboteur =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants

    ///////////////////////////////////Helper functions//////////////////////////////////////
    let calculateDesireAttackJob (j:Job) (s:State) = 
        let ((_,newValue,_,_),(jobData)) = j      
        let oldJobValue = 
                            if (s.MyJobs.IsEmpty) then
                                0
                            else
                                (getJobValueFromJoblist s.MyJobs s)

        let jobTargetNode = 
            match jobData with
            | AttackJob (zone) -> zone.Head
        

        let (distanceToJob,personalValueMod) = (getDistanceToJobAndNumberOfEnemyNodes jobTargetNode s)
        
        //final desire
        int <| (((float newValue) * personalValueMod) - (float oldJobValue))    +     (-(distanceToJob * DISTANCE_TO_ATTACK_JOB_MOD))    +    SABOTEUR_ATTACKJOB_MOD

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let applyToAttackJob (s:State) = 
        let applicationList = createApplicationList s JobType.AttackJob calculateDesireAttackJob
        Some(
                "apply to all attack jobs"
                , Communication
                , [Plan(fun state -> applicationList)]
            )

    let spontanouslyAttackAgentOnMyNode (s:State) = 
        let enemiesNearby = List.filter (fun a -> a.Node = s.Self.Node) s.EnemyData
        match enemiesNearby with
        | [] -> None
        | head::tail ->     
            Some(
                    "attack agent " + head.Name
                    , Activity
                    , [Requirement(fun state -> agentHasFulfilledRequirementEnemies head.Name state (fun ag -> ag.Status = EntityStatus.Disabled) )]
                )
    
    let workOnAttackJob (s:State) = None
    
    let spontanouslyAttackAgent (s:State) = 
        let enemiesNearby = List.filter (fun a -> a.Status <> Disabled) (nearbyEnemies s s.Self)
        match enemiesNearby with
        | [] -> None
        | head::tail ->     
            Some(
                    "attack agent " + head.Name
                    , Activity
                    , [Requirement(fun state -> agentHasFulfilledRequirementEnemies head.Name state (fun ag -> ag.Status = EntityStatus.Disabled))]
                )
             
    
    let applyToDisruptJob (s:State) = None //advanced feature
    
    let workOnDisruptJobThenParryIfEnemiesClose (s:State) = None //advanced feature
    
    let findAgentToDestroy (s:State) = 
        Some(
                "find and destroy an agent"
                , Activity
                , [Requirement(
                    fun state ->  
                        match s.LastAction with
                        | (Attack _) -> true
                        | _ -> false
                )]
            )