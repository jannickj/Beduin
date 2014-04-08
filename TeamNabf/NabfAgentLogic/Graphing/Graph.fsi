namespace Graphing
    module Graph =
    
        type Edge = Option<int> * string * string
        type DirectedEdge = Option<int> * string

        type VertexName = string

        type Vertex = 
            {
                Identifier : VertexName;
                Value : Option<int>;
                Edges : Set<DirectedEdge>;
            }

        type Graph = Map<string, Vertex>

        val getNeighbourIds : VertexName -> Graph -> VertexName list
        val getNeighbours : string -> Graph -> Vertex list
        val isVertexAdjacentTo : string -> Vertex -> bool
        val addEdgeToVertex : string -> DirectedEdge -> Graph -> Graph
        val addVertex : Graph -> Vertex -> Graph
        val addEdge : Edge -> Graph -> Graph
        val addVertexValue : string -> int -> Graph -> Graph
        val removeVertex : Vertex -> Graph -> Graph
        val join : Graph -> Graph -> Graph
        val addVertexById : string -> Graph -> Graph