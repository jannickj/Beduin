namespace NabfAgentLogic
module GeneralLib =    
    open AgentTypes
    open Constants
    open Logging

    let flip f x y = f y x

    let agentsHere vertex agentList =
        List.filter (fun agent -> agent.Node = vertex) agentList

    let alliesHere state vertex =
        agentsHere vertex state.FriendlyData

    let enemiesHere state vertex = 
        agentsHere vertex state.EnemyData

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
    let tryFindAgentsByName name agentList =
        match List.partition (getAgentName >> ((=) name)) agentList with
        | [agent],others ->
            Some (agent,others)
        | [],others ->
            None
        | agent::_,others ->
            Some(agent,others)

    let updateAgentPosition agentName vertexName agentList =
        let friendly = tryFindAgentsByName agentName agentList
        match friendly with
        | Some (agent,others) -> 
            ({ agent with Node = vertexName })::others
        | None -> 
            { buildAgent vertexName OUR_TEAM with Node = vertexName }::agentList
