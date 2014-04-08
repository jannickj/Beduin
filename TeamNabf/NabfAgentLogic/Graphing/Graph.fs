namespace Graphing
    module Graph =
    
        type Edge = Option<int> * string * string
        type DirectedEdge = Option<int> * string

        type VertexName = string

        type Vertex = 
            {
                Identifier : string;
                Value : Option<int>;
                Edges : Set<DirectedEdge>;
            }

        type Graph = Map<string, Vertex>

        let getNeighbourIds vertexId (graph : Graph) =
            let nEdges = Set.toList graph.[vertexId].Edges
            let neighbourName  =(snd (List.unzip nEdges))
            List.filter (fun id -> graph.ContainsKey id) neighbourName

        let getNeighbours vertexId (graph:Graph) = 
            List.map (fun id -> graph.[id]) <| getNeighbourIds vertexId graph 


        let isVertexAdjacentTo identifier vertex = 
            Set.exists (fun (_, toVertex) -> toVertex = identifier) vertex.Edges 

        let addEdgeToVertex (vertexId : string) ((value, otherVertexId) : DirectedEdge) (graph : Graph) =
            let vertex = graph.[vertexId]
            let updatedVertex = 
                { vertex with Edges = 
                              Set.filter (fun (_, id) -> id <> otherVertexId) vertex.Edges
                              |> Set.add (value, otherVertexId) 
                } 
            Map.add vertex.Identifier updatedVertex graph

        let rec updateGraph (vertex : Vertex) (graph : Graph) = 
            match Set.toList vertex.Edges with
            | (weight, otherVertexId) :: rest -> 
                (if graph.ContainsKey(otherVertexId) then
                    addEdgeToVertex otherVertexId (weight, vertex.Identifier) graph
                else
                    graph)
                |> updateGraph { vertex with Edges = vertex.Edges.Remove (weight, otherVertexId) }
            | [] -> graph

        let addVertex graph vertex = 
            updateGraph vertex graph |> Map.add vertex.Identifier vertex

        let addVertexById vertexId graph =
            if Map.containsKey vertexId graph then
                graph
            else
                Map.add vertexId {Identifier = vertexId; Value = None; Edges = Set.empty} graph

        let addEdge ((weight, vertexId1, vertexId2) : Edge) (graph : Graph) =
            let graph' = 
                addVertexById vertexId1 graph 
                |> addVertexById vertexId2
            addEdgeToVertex vertexId1 (weight, vertexId2) graph'
            |> addEdgeToVertex vertexId2 (weight, vertexId1)

        let join (graph : Graph) (graph' : Graph) = 
            Map.empty<string, Vertex> : Graph

        let addVertexValue vertex value (graph : Graph) =
            let newGraph = addVertexById vertex graph
            Map.add vertex { newGraph.[vertex] with Value = Some value } newGraph
        
        let removeEdgeFromVertex edge vertex =
            { vertex with Edges = Set.remove edge vertex.Edges }

        let removeVertex vertex (graph : Graph) =
            let updateVertex (cost, id) (graph : Graph) = 
                Map.add id (removeEdgeFromVertex (cost, vertex.Identifier) graph.[id]) graph
            
             
            vertex.Edges
            |> Set.fold (fun graph edge -> updateVertex edge graph) graph
            |> Map.remove vertex.Identifier



