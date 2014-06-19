namespace NabfAgentLogic
module GoalSpecifications =

    open AgentTypes
    open Graphing.Graph
    open LogicLib
    open Constants
    open Logging
    open GeneralLib

    let agentAt agentName state =
        match tryFindAgentByName agentName (state.EnemyData @ state.FriendlyData) with
        | Some agent when agent.Node <> "" ->
            Some agent.Node 
        | _ -> None

    let agentAttacked agent state = 
        let enemy = List.find (fun (enemy : Agent) -> enemy.Name = agent) state.EnemyData
        enemy.Status = Disabled

    let vertexProbed vertex state = 
//        logStateImportant state Planning <| sprintf "vertex probed %A" state.World.[vertex]
        Option.isSome state.World.[vertex].Value

    let vertexInspected vertex state =
        List.forall (fun enemy -> Option.isSome enemy.Role) (enemiesHere state vertex)

    let parried state = 
        state.LastAction = Parry
    
    let charged charge (state : State) =
        match charge with
        | Some charge -> charge >= state.Self.Energy.Value
        | None -> state.LastAction = Recharge

    let agentRepaired agent state =
        match List.tryFind (fun a -> a.Name = agent) state.FriendlyData with
        | Some (agent) -> agent.Status = Normal
        | None -> false
        //state.LastAction = Repair agent
    
    let getCloseTo (agentName:AgentName) (state:State) =
        match tryFindAgentByName agentName state.FriendlyData with
        | Some ({Node = vn}) when vn <> "" && Map.containsKey vn state.World-> 
            let nodes = vn::(getNeighbourIds vn state.World)
            List.exists ((=) state.Self.Node) nodes
        | _ -> 
            failwith ("Cannot reach state get close "+agentName+" as it does not exist")
            false    

    let atMinValueNode value state = 
        let n = state.World.[state.Self.Node] 
        if (n.Value.IsSome && nodeHasNoOtherFriendlyAgentsOnIt state n.Identifier) then
            n.Value.Value >= value
        else
            false

    let surveyedNeighbourEdges state = 
        let rangeOneEdges = state.World.[state.Self.Node].Edges          
        0 = (Set.count <| Set.filter (fun (value,_) -> Option.isNone value) rangeOneEdges)

    let generateGoalCondition goal =
        match goal with
        | At vertex 
        | Explored vertex -> fun state -> state.Self.Node = vertex
        | Attacked agent -> agentAttacked agent
        | Probed vertex -> vertexProbed vertex
        | Inspected vertex -> vertexInspected vertex
        | Parried -> parried
        | Charged charge -> charged charge
        | AtMinValueNode value -> atMinValueNode value
        | Repaired agent -> agentRepaired agent
        | GetCloseTo agent -> getCloseTo agent
        | Surveyed -> surveyedNeighbourEdges

    let distanceHeuristics vertex =
        distanceBetweenAgentAndNode vertex
    
    let agentDistanceHeuristics agent state =
        match agentAt agent state with
        | Some vn -> distanceHeuristics vn state
        | _ -> 0


    let goalHeuristics goal =
        match goal with
        | At vertex 
        | Explored vertex
        | Probed vertex
        | Inspected vertex -> 
            distanceHeuristics vertex
        
        | GetCloseTo agent
        | Attacked agent 
        | Repaired agent ->
            agentDistanceHeuristics agent

        | Charged _
        | AtMinValueNode _
        | Surveyed
        | Parried-> fun _ -> 0


    let goalVertex goal state =
        match goal with
        | At vertex 
        | Explored vertex
        | Inspected vertex
        | Probed vertex ->
            Some <| vertex
        
        | Attacked agent 
        | Repaired agent ->
            agentAt agent state

        | _ -> None
