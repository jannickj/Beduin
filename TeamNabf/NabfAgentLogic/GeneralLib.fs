namespace NabfAgentLogic
module GeneralLib =    
    open AgentTypes

    let flip f x y = f y x

    let agentsHere vertex agentList =
        List.filter (fun agent -> agent.Node = vertex) agentList

    let alliesHere state vertex =
        agentsHere vertex state.FriendlyData

    let enemiesHere state vertex = 
        agentsHere vertex state.EnemyData

    
