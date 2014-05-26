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


    let nodeHasEnemyAgent (state:State) node =
        List.exists (fun a -> a.Node = node) state.EnemyData
    ////////////////////////////////////////Logic////////////////////////////////////////////

    let applyToAttackJob (inputState:State) = 
        let applicationList = createApplicationList inputState JobType.AttackJob calculateDesireAttackJob
        Some(
                "apply to all attack jobs"
                , Communication
                , [Plan(fun state -> applicationList)]
            )

    let spontanouslyAttackAgentOnMyNode (inputState:State) = 
        let enemiesNearby = List.filter (fun a -> a.Node = inputState.Self.Node) inputState.EnemyData
        match enemiesNearby with
        | [] -> None
        | head::tail ->     
            Some(
                    "attack agent " + head.Name
                    , Activity
                    , [Requirement(
                                        ((fun state -> agentHasFulfilledRequirementEnemies head.Name state (fun ag -> ag.Status = EntityStatus.Disabled)), None) 
                                    )]
                )
    
    let workOnAttackJob (inputState:State) = None
    
    let spontanouslyAttackAgent (inputState:State) = 
        let enemiesNearby = List.filter (fun a -> a.Status <> Disabled) (nearbyEnemies inputState inputState.Self)
        match enemiesNearby with
        | [] -> None
        | head::tail ->     
            Some(
                    "attack agent " + head.Name
                    , Activity
                    , [Requirement(
                                    ((fun state -> agentHasFulfilledRequirementEnemies head.Name state (fun ag -> ag.Status = EntityStatus.Disabled)), None)
                    
                    )]
                )
             
    
    let applyToDisruptJob (inputState:State) = None //advanced feature
    
    let workOnDisruptJobThenParryIfEnemiesClose (inputState:State) = None //advanced feature
    
    let findAgentToDestroy (inputState:State) = 
        findAndDo inputState.Self.Node nodeHasEnemyAgent "attack an agent" false inputState
//        Some(
//                "find and destroy an agent"
//                , Activity
//                , [Requirement(
//                    fun state ->  
//                        match state.LastAction with
//                        | (Attack _) -> true
//                        | _ -> false
//                )]
//            )