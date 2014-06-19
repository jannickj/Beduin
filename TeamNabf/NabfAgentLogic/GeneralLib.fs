namespace NabfAgentLogic
module GeneralLib =    
    open AgentTypes
    open Constants
    open Logging
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
    let getAgentName (a:Agent) = a.Name

    let buildAgent name team =
        { Energy      = None
        ; Health      = None
        ; MaxEnergy   = None
        ; MaxEnergyDisabled = None
        ; MaxHealth   = None
        ; Name        = name
        ; Node        = ""
        ; Role        = None
        ; RoleCertainty = 100
        ; Strength    = None
        ; Team        = team
        ; VisionRange = None
        ; Status      = Normal
        }
    
    //Returns a tuple of the agent with the given name and the other agents
    let tryPartionAgentsByName name agentList =
        match List.partition (getAgentName >> ((=) name)) agentList with
        | [agent],others ->
            Some (agent,others)
        | [],others ->
            None
        | agent::_,others ->
            Some(agent,others)

    let tryFindAgentByName name agentlist =
        match tryPartionAgentsByName name agentlist with
        | Some ans -> Some <| fst ans
        | None -> None

    let updateAgentPosition agentName vertexName agentList =
        let friendly = tryPartionAgentsByName agentName agentList
        match friendly with
        | Some (agent,others) -> 
            ({ agent with Node = vertexName })::others
        | None -> 
            { (buildAgent agentName OUR_TEAM) with Node = vertexName }::agentList
