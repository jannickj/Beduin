
namespace AgentLogicTest
module DijkstraTest =
    open System
    open NUnit.Framework
    open NabfAgentLogic.AgentTypes
    open Graphing.Graph
    open Graphing.Dijkstra
    open NabfAgentLogic.PathFinding

    [<TestFixture>]
    type GraphTest() = 

            let testAgent =
                { Energy      = Some 10
                ; Health      = None
                ; MaxEnergy   = Some 20
                ; MaxHealth   = None
                ; Name        = "testAgent"
                ; Node        = "a"
                ; Role        = None
                ; Strength    = None
                ; Team        = "testTeam"
                ; VisionRange = None
                ; Status      = Normal
                }
            
            [<Test>]
            member this.FindPath_SimpleGraph_FindCorrectPath () =

                let graph = 
                    [ ("a", { Identifier = "a"; Value = None; Edges = [(None, "c"); (None, "b")] |> Set.ofList}) 
                    ; ("b", { Identifier = "b"; Value = None; Edges = [(None, "c"); (None, "a")] |> Set.ofList})
                    ; ("c", { Identifier = "c"; Value = None; Edges = [(None, "a"); (None, "b")] |> Set.ofList}) 
                    ] |> Map.ofList

                let correctPath = Some ["c"]

                let actualPath = pathTo testAgent "c" graph

                Assert.AreEqual (correctPath, actualPath)

            [<Test>]
            member this.FindPath_AlreadyThere_ReturnEmptyPath () =

                let graph = [ ("a", { Identifier = "a"; Value = None; Edges = [(None, "c"); (None, "b")] |> Set.ofList}); 
                              ("b", { Identifier = "b"; Value = None; Edges = [(None, "c"); (None, "a")] |> Set.ofList});
                              ("c", { Identifier = "c"; Value = None; Edges = [(None, "a"); (None, "b")] |> Set.ofList}) 
                            ] |> Map.ofList

                let path = pathTo testAgent "a" graph

                Assert.IsEmpty(path.Value)

            [<Test>]
            member this.FindPath_NoPathExists_ReturnsNone () =

                let graph = [ ("a", { Identifier = "a"; Value = None; Edges = [] |> Set.ofList});
                              ("b", { Identifier = "b"; Value = None; Edges = [] |> Set.ofList}) 
                            ] |> Map.ofList
                 
                let expected = None
                let actual = pathTo testAgent "b" graph

                Assert.AreEqual (expected, actual)

            [<Test>]
            member this.FindPath_TwoPossiblePaths_ReturnsShortestPath () =
           
               //        B------C     
               //       /        \    Find a path
               //      /          \   from A to E
               //     /            \
               //    A-------D------E

                let graph = 
                    [ ("a", { Identifier = "a"; Value = None; Edges = [(None, "b"); (None, "d")] |> Set.ofList}) 
                    ; ("b", { Identifier = "b"; Value = None; Edges = [(None, "a"); (None, "c")] |> Set.ofList}) 
                    ; ("c", { Identifier = "c"; Value = None; Edges = [(None, "b"); (None, "e")] |> Set.ofList}) 
                    ; ("d", { Identifier = "d"; Value = None; Edges = [(None, "a"); (None, "e")] |> Set.ofList}) 
                    ; ("e", { Identifier = "e"; Value = None; Edges = [(None, "c"); (None, "d")] |> Set.ofList}) ] |> Map.ofList

                let expected = Some ["d"; "e"]
                let actual = pathTo testAgent "e" graph
                 
                Assert.AreEqual (expected, actual)

            [<Test>]
            member this.FindPath_TwoPathsWithEdgeCosts_ReturnsFastestPath () =
           
                //          
                //            10
                //     B---------------C         
                //     |                |       Find a path
                //  10 |                | 10    from A to G
                //     |                |
                //     A---D---E---F---G
                //       1   1   1   1

                let graph = [ ("a", { Identifier = "a"; Value = None; Edges = [(Some 10, "b"); (Some 1, "d")] |> Set.ofList}); 
                              ("b", { Identifier = "b"; Value = None; Edges = [(Some 10, "a"); (Some 10, "c")] |> Set.ofList}); 
                              ("c", { Identifier = "c"; Value = None; Edges = [(Some 10, "b"); (Some 10, "g")] |> Set.ofList}); 
                              ("d", { Identifier = "d"; Value = None; Edges = [(Some 1, "a"); (Some 1, "e")] |> Set.ofList}); 
                              ("e", { Identifier = "e"; Value = None; Edges = [(Some 1, "d"); (Some 1, "f")] |> Set.ofList});
                              ("f", { Identifier = "f"; Value = None; Edges = [(Some 1, "e"); (Some 1, "g")] |> Set.ofList});
                              ("g", { Identifier = "g"; Value = None; Edges = [(Some 1, "f"); (Some 10, "c")] |> Set.ofList}); ] |> Map.ofList

                let expected = Some ["d"; "e"; "f"; "g"]
                let actual = pathTo testAgent "g" graph

                Assert.AreEqual (expected, actual)
            
            [<Test>]
            member this.FindPath_TwoPathsThatTakeTheSameAmountOfTurnsPart1_ReturnsPathWithHighestFinalEnergy () =

                //
                //          B
                //      2  / \  2
                //        /   \  
                //       A     D        Find a path
                //        \   /         from A to D
                //      1  \ /  1
                //          C

                let graph = [ ("a", { Identifier = "a"; Value = None; Edges = [(Some 2, "b"); (Some 1, "c")] |> Set.ofList}); 
                               ("b", { Identifier = "b"; Value = None; Edges = [(Some 2, "a"); (Some 2, "d")] |> Set.ofList}); 
                               ("c", { Identifier = "c"; Value = None; Edges = [(Some 1, "a"); (Some 1, "d")] |> Set.ofList}); 
                               ("d", { Identifier = "d"; Value = None; Edges = [(Some 2, "b"); (Some 1, "c")] |> Set.ofList}); ] |> Map.ofList

                let expected = Some ["c"; "d"]
                let actual = pathTo testAgent "d" graph

                Assert.AreEqual (expected, actual)


            [<Test>]
            member this.FindPath_TwoPathsThatTakeTheSameAmountOfTurnsPart2_ReturnsPathWithHighestFinalEnergy () =
               
                //
                //          B
                //      1  / \  1
                //        /   \  
                //       A     D        Find a path
                //        \   /         from A to D
                //      2  \ /  2
                //          C

                let graph = [ ("a", { Identifier = "a"; Value = None; Edges = [(Some 1, "b"); (Some 2, "c")] |> Set.ofList}); 
                               ("b", { Identifier = "b"; Value = None; Edges = [(Some 1, "a"); (Some 1, "d")] |> Set.ofList}); 
                               ("c", { Identifier = "c"; Value = None; Edges = [(Some 2, "a"); (Some 2, "d")] |> Set.ofList}); 
                               ("d", { Identifier = "d"; Value = None; Edges = [(Some 1, "b"); (Some 2, "c")] |> Set.ofList}); ] |> Map.ofList

                let expected = Some ["b";"d"]
                let actual = pathTo testAgent "d" graph

                Assert.AreEqual (expected, actual)

            [<Test>]
            member this.FindPath_PathWithLowestCostsIsLongest_ReturnsShortestPath () =
           
                //                          
                //             2            
                //     B---------------C         
                //     |                |       Find a path
                //   2 |                | 2     from A to G
                //     |                |                   
                //     A---D---E---F---G                    
                //       1   1   1   1                      

                let graph = [ ("a", { Identifier = "a"; Value = None; Edges = [(Some 2, "b"); (Some 1, "d")] |> Set.ofList}); 
                              ("b", { Identifier = "b"; Value = None; Edges = [(Some 2, "a"); (Some 2, "c")] |> Set.ofList}); 
                              ("c", { Identifier = "c"; Value = None; Edges = [(Some 2, "b"); (Some 2, "g")] |> Set.ofList}); 
                              ("d", { Identifier = "d"; Value = None; Edges = [(Some 1, "a"); (Some 1, "e")] |> Set.ofList}); 
                              ("e", { Identifier = "e"; Value = None; Edges = [(Some 1, "d"); (Some 1, "f")] |> Set.ofList});
                              ("f", { Identifier = "f"; Value = None; Edges = [(Some 1, "e"); (Some 1, "g")] |> Set.ofList});
                              ("g", { Identifier = "g"; Value = None; Edges = [(Some 1, "f"); (Some 2, "c")] |> Set.ofList}); ] |> Map.ofList

                let expected = Some ["b"; "c"; "g"]
                let actual = pathTo testAgent "g" graph

                Assert.AreEqual (expected, actual)
            
            [<Test>]
            member this.RangeTo_CyclicGraph_LengthOfShortestPath () =
                
                (*                         
                 *             2            
                 *     B---------------C         
                 *     |                |       
                 *   2 |                | 2    
                 *     |                |                   
                 *     A---D---E---F---G                    
                 *       1   1   1   1
                 *)

                let graph = 
                    [ ("a", { Identifier = "a"; Value = None; Edges = [(Some 2, "b"); (Some 1, "d")] |> Set.ofList}) 
                    ; ("b", { Identifier = "b"; Value = None; Edges = [(Some 2, "a"); (Some 2, "c")] |> Set.ofList}) 
                    ; ("c", { Identifier = "c"; Value = None; Edges = [(Some 2, "b"); (Some 2, "g")] |> Set.ofList}) 
                    ; ("d", { Identifier = "d"; Value = None; Edges = [(Some 1, "a"); (Some 1, "e")] |> Set.ofList}) 
                    ; ("e", { Identifier = "e"; Value = None; Edges = [(Some 1, "d"); (Some 1, "f")] |> Set.ofList})
                    ; ("f", { Identifier = "f"; Value = None; Edges = [(Some 1, "e"); (Some 1, "g")] |> Set.ofList})
                    ; ("g", { Identifier = "g"; Value = None; Edges = [(Some 1, "f"); (Some 2, "c")] |> Set.ofList}) 
                    ] |> Map.ofList

                let expected = 3
                let actual = distanceTo testAgent "g" graph

                Assert.AreEqual (expected, actual)

            [<Test>]
            member this.FindNearestUnProbed_GraphWithTwoUnprobedVertices_FindsPathToTheNearest () =
                (* 
                 *     A
                 *    /|        Starting in A,
                 * 4 / | 3      B and C are unprobed
                 *  /  |
                 * B---C
                 *   *
                 *)

                let graph =
                    [ ("a", { Identifier = "a"; Value = Some 1; Edges = [(Some 4, "b"); (Some 3, "c")] |> Set.ofList })
                    ; ("b", { Identifier = "b"; Value = None; Edges = [(Some 4, "a"); (None, "c")] |> Set.ofList })
                    ; ("c", { Identifier = "c"; Value = None; Edges = [(Some 3, "a"); (None, "b")] |> Set.ofList })
                    ] |> Map.ofList
                
                let expected = Some ["c"]
                let actual = pathToNearestUnProbed testAgent graph

                Assert.AreEqual (expected, actual)

            [<Test>]
            member this.FindNearestUnExplored_GraphWithTwoUnExploredVertices_FindsPathToTheNearest () =
                (*
                 *  A---B       
                 *    * 
                 *
                 * The agent starts at vertex A. Neither A nor B have any edges with known weights.
                 * The nearest vertex that do not have any edges with known weights will therefore
                 * be vertex A, but it is already explored. Here we check that the the function
                 * correctly returns B. This case is only relevant in the very first turn.
                 *
                 *)
                
                let graph = 
                    [ ("a", { Identifier = "a"; Value = None; Edges = [(None, "b")] |> Set.ofList })
                    ; ("b", { Identifier = "b"; Value = None; Edges = [(None, "a")] |> Set.ofList })
                    ] |> Map.ofList

                let expected = Some ["b"] 
                let actual = pathToNearestUnExplored testAgent graph

                Assert.AreEqual (expected, actual)

            [<Test>]
            member this.PathsToNearestNUnexplored_3VerticesInSameRadius_PathsToThreeVertices () =
                (*
                 *     *     *    
                 *  C --- A --- B
                 *        |
                 *        | *
                 *        |
                 *        D
                 *)

                let graph = 
                    [ ("a", { Identifier = "a"; Value = None; Edges = [(None, "b"); (None, "c"); (None, "d")] |> Set.ofList }) 
                    ; ("b", { Identifier = "b"; Value = None; Edges = [(None, "a")] |> Set.ofList })
                    ; ("c", { Identifier = "c"; Value = None; Edges = [(None, "a")] |> Set.ofList })
                    ; ("d", { Identifier = "d"; Value = None; Edges = [(None, "a")] |> Set.ofList })
                    ] |> Map.ofList

                let expected = [["d"];["c"];["b"]]

                let actual = pathsToNearestNUnexplored 3 testAgent graph

                Assert.AreEqual (expected, actual)

            [<Test>]
            member this.PathToNearestNUnExplored_2VerticesInSameRadius1VertexFartherAway_PathsTo2ClosestVertices () =
                (*
                 *     *     5     *
                 *  C --- A --- B --- E
                 *        |
                 *        | *
                 *        |
                 *        D
                 *)

                let graph = 
                   [ ("a", { Identifier = "a"; Value = None; Edges = [(Some 5, "b"); (None, "c"); (None, "d")] |> Set.ofList }) 
                   ; ("b", { Identifier = "b"; Value = None; Edges = [(Some 5, "a"); (None, "e")] |> Set.ofList })
                   ; ("c", { Identifier = "c"; Value = None; Edges = [(None, "a")] |> Set.ofList })
                   ; ("d", { Identifier = "d"; Value = None; Edges = [(None, "a")] |> Set.ofList })
                   ; ("e", { Identifier = "e"; Value = None; Edges = [(None, "b")] |> Set.ofList })
                   ] |> Map.ofList

                let expected = [["d"];["c"]]

                let actual = pathsToNearestNUnexplored 3 testAgent graph

                Assert.AreEqual (expected, actual)
