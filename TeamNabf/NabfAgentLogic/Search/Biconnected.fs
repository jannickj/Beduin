namespace NabfAgentLogic.Search
module Biconnected =
    
    open HeuristicDijkstra
    open Graphing.Graph

    let getcomponents world allsets vertex =
        let isInSet = List.exists (Set.contains vertex) allsets
        if isInSet then
            allsets
        else
            let allpairs = allDistances world vertex
            let graphSubSet = List.fold (fun subset (_,vn) -> Set.add vn subset) (Set [vertex]) allpairs
            graphSubSet::allsets

    let find vertex world : Set<VertexName> list =
        let comWorld = Map.remove vertex world
        let v:Vertex = Map.find vertex world
        let neighbors = getNeighbourIds vertex world
        List.fold (getcomponents comWorld) [] neighbors
