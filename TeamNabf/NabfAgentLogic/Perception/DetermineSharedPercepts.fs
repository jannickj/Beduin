namespace NabfAgentLogic.Perception
module DetermineSharedPercepts =
    open NabfAgentLogic.AgentTypes
    open NabfAgentLogic.GeneralLib

    let shouldSharePercept oldState (state:State) percept = 
        match percept with
        | AgentRolePercept (_,_,_) -> true
        | NodeKnowledge (name, None) ->
            not <| Set.contains name oldState.ExploredNodes
        | VertexProbed (vertexName, value) ->
            match Map.tryFind vertexName oldState.World with
            | Some vertex -> Option.isNone vertex.Value
            | None -> true
            
        | EdgeSeen (edgeValue, node1, node2) ->
            if oldState.World.ContainsKey(node1) then
                let edge = Set.filter (fun (_, endNode) -> endNode = node2) oldState.World.[node1].Edges
                if edge.Count = 1 then 
                                    match edge.MaximumElement with
                                    | (value, _) -> value.IsNone && edgeValue.IsSome
                elif edge.Count = 0 then true
                else
                    failwith <| sprintf "Handle edge seen percept - found %A of the given edge in the world." (string edge.Count)
            else
                true
          

        | InspectedEntity { Role = role ; Name = name} ->
            match tryFindAgentByName name oldState.EnemyData with
            | Some oldAgent -> Option.isNone oldAgent.Role
            | None -> true

        | _ -> false

    let selectSharedPercepts percepts oldState (state:State) =
        let propagatedPercepts = List.filter (shouldSharePercept oldState state) percepts
        { state with 
                NewKnowledge = state.NewKnowledge @ propagatedPercepts
        }

