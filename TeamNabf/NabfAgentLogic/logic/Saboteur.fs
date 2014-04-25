namespace NabfAgentLogic
module Saboteur =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib

    ///////////////////////////////////Helper functions//////////////////////////////////////
    let calculateDesireAttackJob (j:Job) (s:State) = 
        let ((_,value,_,_),_) = j
        value
   

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let applyToAttackJob (s:State) = 
        let applicationList = createApplicationList s JobType.AttackJob calculateDesireAttackJob
        Some(
                "apply to all attack jobs"
                , Communication
                , [Plan(fun state -> applicationList)]
            )
    
    let doAttackJob (s:State) = None
    
    let spontanousAttackAgent (s:State) = 
        let enemiesNearby = List.filter (fun a -> true) (nearbyEnemies s s.Self)
        match enemiesNearby with
        | [] -> None
        | head::tail ->     
            Some(
                    "attack agent " + head.Name
                    , Activity
                    , [Requirement(fun state -> agentHasFulfilledRequirement head.Name state (fun ag -> ag.Status = EntityStatus.Disabled))]
                )
             
    
    let applyToDisruptJob (s:State) = None //advanced feature
    
    let doDisruptJobThenParryIfEnemiesClose (s:State) = None //advanced feature
    
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