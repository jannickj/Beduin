namespace NabfAgentLogic
    module PathFinding = 
        open Graphing.Graph
        open Graphing.Dijkstra
        open AgentTypes

//        [<CustomComparison; CustomEquality>]
//        type PathCost = 
//            { Steps : int
//            ; Energy : int
//            }
//            with
//            interface System.IComparable<PathCost> 
//            interface System.IComparable
//            interface System.IEquatable<PathCost>

        val pathToNearest : Agent -> (VertexName -> bool) -> Graph -> (VertexName list) option
        val pathTo : Agent -> string -> Graph -> (string list) option
        val distanceTo : Agent -> string -> Graph -> int
        val pathToNearestUnProbed : Agent -> Graph -> (string list) option
        val pathToNearestUnExplored : Agent -> Graph -> (string list) option
        val pathsToNearestNUnexplored : int -> Agent -> Graph -> VertexName list list
