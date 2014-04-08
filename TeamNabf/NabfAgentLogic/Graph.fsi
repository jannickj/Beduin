namespace Graphing
    module Graph = 
    
        type Edge = Option<int> * string * string
        type DirectedEdge = Option<int> * string

        type Vertex = 
            {
                Identifier : string;
                Value : Option<int>;
                Edges : Set<DirectedEdge>;
            }

        type Graph = Map<string, Vertex>

        val getNeighbours : string -> Graph -> Vertex list
        val addVertexValue : Graph -> Vertex -> Graph
        val addEdgeCost : Graph -> Edge -> Graph
        val addVertex : Graph -> Vertex -> Graph
        val addEdge : Graph -> Edge -> Graph
        val join : Graph -> Graph -> Graph