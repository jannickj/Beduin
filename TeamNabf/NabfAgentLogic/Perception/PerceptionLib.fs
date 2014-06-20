namespace NabfAgentLogic.Perception
module PerceptionLib =
    
    open NabfAgentLogic.AgentTypes
    open NabfAgentLogic.GeneralLib
    open Graphing.Graph

    let updateAgentWithEntdata (name,team,node,status) canSeeIt agentOld =                       
        match agentOld with
        | Some agent ->
            { agent with 
                Node = node 
                Team = team
                Status = status
                IsInVisionRange = canSeeIt
            }
        | None -> { buildAgent name team canSeeIt with Node = node; Status = status }
    
    let updateAgentWithAgent (agent:Agent) agentOld =
        agent            
    
    let updateAgentWithRole name role certainty canSeeIt agentOld =
        match agentOld with
        | Some agent ->
            { agent with
                Role = Some role
                RoleCertainty = certainty
            }
        | None -> { buildAgent name "evil" canSeeIt with Role = Some role; RoleCertainty = certainty }

    let updateAgentList name updater alist = 
        match tryPartionAgentsByName name alist with 
        | None -> (updater None)::alist
        | Some(oldAgent,others) -> (updater (Some oldAgent))::others    
  
    
    let updateAgentListsOnState name team updater state =             
        if team = state.Self.Team then
            { state with FriendlyData = updateAgentList name updater state.FriendlyData }
        else
            { state with EnemyData = updateAgentList name updater state.EnemyData }


    let rec addListToMap mapToUpdate list =
        match list with 
        | (key,value)::tail -> 
                                let newMap = Map.add key value mapToUpdate
                                addListToMap newMap tail
        | [] -> mapToUpdate

    let rec addListOfMapsToMap mapToUpdate list =
        match list with 
        | head::tail -> 
                        let newMap = addListToMap mapToUpdate (Map.toList head)
                        addListOfMapsToMap newMap tail
        | [] -> mapToUpdate

    
    let updateEdgeOnWorld edge (world:Graph) =
        let (cost, node1, node2) = edge
        match cost, Map.tryFind node1 world with
        | None, Some (v1) -> 
            let edgeHasCost isNode (cost, node) = isNode = node && Option.isSome cost
            //checks if there exists an edge already with cost
            if Set.exists (edgeHasCost node2) v1.Edges then
                world
            else
                addEdge edge world
        | _ ->
            addEdge edge world

    let updateAgentPosition agentName vertexName team isPositionVisible agentList =
        let friendly = tryPartionAgentsByName agentName agentList
        match friendly with
        | Some (agent,others) ->
            if agent.IsInVisionRange && not isPositionVisible then
                agent::others
            else
                ({ agent with Node = vertexName; IsInVisionRange = isPositionVisible })::others
        | None -> 
            { (buildAgent agentName team isPositionVisible) with Node = vertexName }::agentList
    
