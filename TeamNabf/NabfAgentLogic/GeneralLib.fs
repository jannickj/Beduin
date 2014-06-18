namespace NabfAgentLogic
module GeneralLib =    
    open AgentTypes
    open Graphing.Graph

    let flip f x y = f y x

    let agentsHere vertex agentList =
        List.filter (fun agent -> agent.Node = vertex) agentList

    let alliesHere state vertex =
        agentsHere vertex state.FriendlyData

    let enemiesHere state vertex = 
        agentsHere vertex state.EnemyData

    let adjacentDeadEnds state =
        let neighbours = Set.ofList <| getNeighbourIds state.Self.Node state.World
        let isNeighbourOrThis name = Set.contains name neighbours || name = state.Self.Node
        let isDeadEnd vertexName =
            Set.forall (snd >> isNeighbourOrThis) state.World.[vertexName].Edges
        Set.toList <| Set.filter isDeadEnd neighbours

    let isUnexplored state vertex = 
        (not (List.exists (fun (value, _) -> Option.isSome value) <| Set.toList state.World.[vertex].Edges)) && vertex <> state.Self.Node