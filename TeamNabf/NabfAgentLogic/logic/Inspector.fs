namespace NabfAgentLogic
module Inspector =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib

    ///////////////////////////////////Helper functions//////////////////////////////////////

   

    ////////////////////////////////////////Logic////////////////////////////////////////////

    
    let spontanousInspectAgent (s:State) = 
        let uninspectedNearbyEnemies = List.filter (fun a -> a.Role.IsNone) (nearbyEnemies s s.Self)
        match uninspectedNearbyEnemies with
        | [] -> None
        | head::tail ->     
            Some(
                    "inspect agent " + head.Name
                    , Activity
                    , [Requirement(fun state -> agentHasFulfilledRequirement head.Name state (fun ag -> ag.Role.IsSome))]
                )

    let applyToOccupyJob (s:State) = None
    
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