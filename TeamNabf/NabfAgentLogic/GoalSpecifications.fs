namespace NabfAgentLogic
module GoalSpecifications =

    open AgentTypes
    open Graphing.Graph
    open LogicLib
    open Constants

    let agentAt agentName state =
        let agent = List.find (fun ag -> ag.Name = agentName) (state.EnemyData @ state.FriendlyData)
        agent.Node

    let agentAttacked agent state = 
        let enemy = List.find (fun (enemy : Agent) -> enemy.Name = agent) state.EnemyData
        enemy.Status = Disabled

    let vertexProbed vertex state =
        match vertex with
        | Some vertex -> state.World.[vertex].Value.IsSome
        | None -> state.LastAction = (Probe None)

    let explored state = 
        let node = state.Self.Node
        let n = state.World.[node] 
        Set.exists (fun (value : int option, _) -> value.IsSome) n.Edges

    let agentInspected agent state =
        let enemy = List.find (fun enemy -> enemy.Name = agent) state.EnemyData
        enemy.Role.IsSome

    let parried state = 
        state.LastAction = Parry
    
    let recharged state =
        state.LastAction = Recharge

    let agentRepaired agent state =
        state.LastAction = Repair agent

    let occupied state = 
        let n = state.World.[state.Self.Node] 
        if (n.Value.IsSome) then
            n.Value.Value >= MINIMUM_VALUE_VALUE
        else
            false

    let generateGoalCondition goal =
        match goal with
        | At vertex 
        | Explored (Some vertex) -> fun state -> state.Self.Node = vertex
        | Attacked agent -> agentAttacked agent
        | Probed vertex -> vertexProbed vertex
        | Explored None -> explored
        | Inspected agent -> agentInspected agent
        | Parried -> parried
        | Recharged -> recharged
        | Occupied -> occupied
        | Repaired agent -> agentRepaired agent

    let distanceHeuristics vertex =
        distanceBetweenAgentAndNode vertex

    let goalHeuristics goal =
        match goal with
        | At vertex 
        | Explored (Some vertex)
        | Probed (Some vertex) -> 
            distanceHeuristics vertex

        | Attacked agent 
        | Repaired agent
        | Inspected agent -> 
            fun state -> distanceHeuristics (agentAt agent state) state

        | Explored None 
        | Probed None
        | Recharged
        | Occupied
        | Parried-> fun _ -> 0