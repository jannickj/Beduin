namespace Graphing
    module Dijkstra =

        open Graph

        type 'a Problem when 'a : comparison =
            { GoalEvaluator : Vertex -> bool
            ; CostEvaluator : int option -> 'a -> 'a
            ; InitialCost   : 'a
            }

        val dijkstra : Vertex -> 'a Problem -> Graph -> ('a * VertexName list) option