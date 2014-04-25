namespace NabfAgentLogic
module Saboteur =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib

    ///////////////////////////////////Helper functions//////////////////////////////////////
        
   

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let applyToAttackJob (s:State) = None
    
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
             
    
    let applyToDisruptJob (s:State) = None
    
    let doDisruptJobThenParryIfEnemiesClose (s:State) = None
    
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