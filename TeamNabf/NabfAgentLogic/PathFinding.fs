namespace NabfAgentLogic
    module PathFinding = 
        open FsPlanning.Searching
        open Graphing.Graph
        open Graphing.Dijkstra
        open AgentTypes
        open System.Collections.Generic

        [<CustomComparison; CustomEquality>]
        type PathState = 
            { Vertex : VertexName
            ; Energy : int
            }
            with
            override self.Equals(yobj) =
                match yobj with
                | :? PathState as obj  -> self.Vertex = obj.Vertex
                | _ -> false
            
            interface System.IComparable with
                member self.CompareTo yobj = 
                   match yobj with
                    | :? PathState as obj  -> self.Vertex.CompareTo(obj.Vertex)
                    | _ -> 0
                
                

        let edgeToVert (_, otherVertex) = otherVertex
        let vertPath path = List.map edgeToVert path

        let definiteCost cost = match cost with 
                                | Some c -> c
                                | None -> Constants.UNKNOWN_EDGE_COST
                            
        let stepProblem agent (goalEvaluator : VertexName -> bool) (graph : Graph) = 
            { InitialState = { Vertex = agent.Node; Energy = agent.Energy.Value }
            ; GoalTest = fun state -> goalEvaluator (state.Vertex)
            ; Actions = fun state -> graph.[state.Vertex].Edges |> Set.toList
            ; Result = fun state (cost, otherVertex) -> 
                let energyLeft = match state.Energy - definiteCost cost with
                                 | e when e >= 0 -> e
                                 | e -> state.Energy + (agent.MaxEnergy.Value / 2) - definiteCost cost
                { Vertex = otherVertex; Energy = energyLeft }

            ; StepCost = fun state (cost, _) -> if definiteCost cost <= state.Energy then 1 else 2
            }
        
        let rangeProblem agent goalEvaluator graph =
            let stepProb = stepProblem agent goalEvaluator graph
            { stepProb with StepCost = fun _ _ -> 1 }

        let pathToNearestWithCost agent goalEvaluator graph =
            solve aStar <| stepProblem agent goalEvaluator graph

        let pathToNearest agent goalEvaluator graph =
            let solution = pathToNearestWithCost agent goalEvaluator graph
            Option.map (fun sol -> vertPath sol.Path) solution
        
        let pathTo agent goal graph = 
            pathToNearest agent (fun vertex -> vertex = goal) graph

        let distanceTo agent goal graph =
            let solution = solve aStar <| rangeProblem agent ((=) goal) graph
//            Option.map (fun sol -> List.length <| sol.Path) solution
            List.length <| solution.Value.Path // This should be the above commented line

        let pathToNearestUnProbed agent (graph : Graph) =
            pathToNearest agent (fun vertex -> graph.[vertex].Value = None) graph

        let isUnexplored vertex = 
            Set.forall (fun (cost,_) -> cost = None) vertex.Edges

        let pathToNearestUnExplored agent (graph : Graph) =
            let goalEvaluator vertex = isUnexplored graph.[vertex] && graph.[vertex].Identifier <> agent.Node
            pathToNearest agent goalEvaluator graph

        let pathsToNearestNUnexplored n agent graph =
            let rec helper n lastCost goalEvaluator =
                match n with
                | -1 -> []
                | _ -> 
                    let costAndPath = pathToNearestWithCost agent goalEvaluator graph
                    match costAndPath with
                    | Some solution when Some solution.Cost = lastCost || lastCost = None -> 
                        let goalEvaluator' = 
                            fun vertex -> (goalEvaluator vertex) && vertex <> (List.rev >> List.head <| vertPath solution.Path)
                        vertPath solution.Path :: helper (n - 1) (Some solution.Cost) goalEvaluator'
                    | _ -> []

            let goalEvaluator vertex = 
                (isUnexplored graph.[vertex]) && vertex <> agent.Node

            helper n None goalEvaluator

            
