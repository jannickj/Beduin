namespace NabfAgentLogic
module SharedLogic =

    open AgentTypes
    open Graphing.Graph
    open AgentLogicLib
    open Logging
    open PathFinding

    open Graphing.Graph

    let kiteAgents agents state = 

        let (nearbyEnemeies, nearbyFriends) = 
            List.partition (fun agent -> agent.Team <> state.Self.Team) state.NearbyAgents

        let (enemyVertices, friendlyVertices) = 
            ( List.map (fun agent -> agent.Node) nearbyEnemeies |> Set.ofList
            , List.map (fun agent -> agent.Node) nearbyFriends |> Set.ofList
            )
        
        let neighbours = getNeighbours state.Self.Node state.World

        let (enemyNeighbours, otherNeighbours) = 
            List.partition (fun vertex -> Set.contains vertex.Identifier enemyVertices) neighbours
        let (friendlyNeighbours, emptyNeighbours) = 
            List.partition (fun vertex -> Set.contains vertex.Identifier friendlyVertices) otherNeighbours

        let neighbourPartitions = [emptyNeighbours; enemyNeighbours; friendlyNeighbours]
        
        // sort empty neighbours by their degree (in decreasing order)
        let sortNeighbours = List.rev << List.sortBy (fun vertex -> Set.count vertex.Edges)
        
        let shouldGoTo vertex =
            match tryGo vertex state with
            | (true, Some (Goto _)) -> true
            | _ -> false

        let bestNeighbour neighbourSet =
            match List.tryFind shouldGoTo <| sortNeighbours neighbourSet with
            | Some neighbour -> Some <| tryGo neighbour state
            | None -> None

        match List.tryPick bestNeighbour neighbourPartitions with
            | Some result -> result
            | None -> recharge
    
    let workOnOccupyGoal (state : State) = 
        let occupyChooser = function
                            | JobGoal (OccupyGoal vertex) -> Some vertex
                            | _ -> None

        let occupyVertices = List.choose occupyChooser state.Goals
        
        match occupyVertices with
//        | vertex :: _ when vertex = state.Self.Node -> recharge
//        | vertex :: _ when vertex <> state.Self.Node ->  let p = pathTo state.Self vertex state.World
//                                                         match p with
//                                                         | Some (first::_) -> tryGo state.World.[first] state
//                                                         | _ -> (false, None)
        | vertex :: _ -> let p = pathTo state.Self vertex state.World
                         logImportant (sprintf  "%A: occupyVertices path: %A" state.Self.Name p)
                         match p with
                         | Some (first :: _) -> tryGo state.World.[first] state
                         | Some [] -> recharge
                         | None -> (false, None)
        | _ -> (false, None)
            
    let workOnKiteGoal (state : State) =
        let kiteChooser = function
                          | KiteGoal (stepsSince, agents) -> Some (stepsSince, agents)
                          | _ -> None

        let kiteAgentLists = List.choose kiteChooser state.Goals

        match kiteAgentLists with
        | (0, agentsToKite) :: _ -> kiteAgents agentsToKite state
        | (1, _) :: _ -> recharge
        | _ -> (false, None)


            