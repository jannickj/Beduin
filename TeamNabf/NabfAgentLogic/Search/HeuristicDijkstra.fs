namespace NabfAgentLogic.Search
module HeuristicDijkstra =
    open FsPlanning.Search
    open FsPlanning.Search.Problem
    open FsPlanning.Search.Astar
    open Graphing.Graph
    open Graphing
    open NabfAgentLogic.Constants
    open NabfAgentLogic.AgentTypes

    type Action = 
        | Move of VertexName 
    
    let cost (world:Graph) node1 (Move node2) =
        let vertex = world.[node1]
        let ne = Set.filter (fun (_,name) -> name=node2) vertex.Edges
        match Set.toList ne with
        | [((Some c), name)] -> c
        | _ -> MINIMUM_EDGE_COST

    let allDistances world from = 
        let prob =  {
                        InitialState = from
                        GoalTest  = (fun _ -> true)
                        Actions   = (fun a -> List.map (fun b -> Move b ) (Graph.getNeighbourIds a world))
                        Result    = (fun a (Move b) -> b)
                        StepCost  = cost world
                        Heuristic = (fun _ c -> c)
                    } : Problem<_,_,_>
        let a = Astar.aStarAllPaths prob
        Astar.allStatesWithCost a

    let allDistancesMap world from =
        let distances = allDistances world from
        let maplist = List.collect (fun (cost,node) -> [((node,from),cost);((from,node),cost)]) distances
        Map.ofList maplist
    
    let folder (state:Map<VertexName*VertexName,_>) (node,nodesWithCost) = 
        let expand = List.map (fun (cost,onode) -> ((node,onode),cost)) nodesWithCost
        Map.ofList ((Map.toList state)@expand)

    let allPairsDistances world =
        let all = List.map (fun a -> (a,(allDistances world a))) <| List.map fst (Map.toList world)
        List.fold folder Map.empty all

    let updateHeuristic (state:State) forNode =
        let (heuMap,countMap) = state.GraphHeuristic
        match Map.tryFind forNode countMap with
        | Some nodeCount when nodeCount = state.World.Count -> 
            (heuMap,countMap)
        | _ -> 
            let allDist = allDistancesMap state.World forNode
            let newHeu = Map.fold   (fun hMap (v1,v2) heuNum -> 
                                        let [vA;vB] = List.sort [v1;v2]
                                        Map.add (vA,vB) heuNum hMap
                                    ) heuMap allDist
            (newHeu,Map.add forNode state.World.Count countMap)
        