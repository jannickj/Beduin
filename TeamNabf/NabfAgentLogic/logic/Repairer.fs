namespace NabfAgentLogic
module Repairer =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib

    ///////////////////////////////////Helper functions//////////////////////////////////////
        
   

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let spontanousRepairNearbyDamagedAgent (s:State) = 
        let nearbyDamagedAgent = List.filter (fun a -> a.Health.Value < (a.MaxHealth.Value / 2)) (nearbyAllies s)
        match nearbyDamagedAgent with
        | [] -> None
        | head::tail ->     
            Some(
                    "repair agent " + head.Name
                    , Activity
                    , [Requirement(fun state -> agentHasFulfilledRequirement head.Name state (fun ag -> ag.Health = ag.MaxHealth))]
                )

    let applyToRepairJob (s:State) = None
    
    let doRepairJob (s:State) = None