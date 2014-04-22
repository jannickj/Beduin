namespace NabfAgentLogic
module Inspector =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open Graphing.Graph

    ///////////////////////////////////Helper functions//////////////////////////////////////
    let nodeListContains n (nl:string list) =
        (List.tryFind (fun s -> s = n) nl).IsSome

    let agentHasBeenInspected aName state =
        (List.tryFind (fun ag -> ag.Role.IsSome && ag.Name = aName) state.EnemyData).IsSome
   

    ////////////////////////////////////////Logic////////////////////////////////////////////

    
    let spontanousInspect (s:State) = 
        let neighbourNodes = List.append (getNeighbourIds s.Self.Node s.World) [s.Self.Node]
        let nearbyEnemies = List.filter (fun a -> nodeListContains a.Node neighbourNodes) s.EnemyData 
        let uninspectedEnemies = List.filter (fun a -> a.Role.IsNone) nearbyEnemies
        match nearbyEnemies with
        | [] -> None
        | head::tail -> Some("inspect agent" + head.Name, Activity, [Requirement(fun state -> agentHasBeenInspected head.Name state)])

    let applyToOccupyJob (s:State) = None
    
    let doOccupyJob (s:State) = None    
    
    let applyToDisruptJob (s:State) = None //advanced feature
    
    let doDisruptJob (s:State) = None //advanced feature
    
    let findAgentToInspect (s:State) = None