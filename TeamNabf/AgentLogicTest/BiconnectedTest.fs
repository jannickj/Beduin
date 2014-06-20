namespace AgentLogicTest
module BiconnectedTest =
    
    open System
    open NUnit.Framework
    open Graphing.Graph
    open NabfAgentLogic.AgentTypes
    open StructureBuilder
    open NabfAgentLogic.Explorer
    open NabfAgentLogic.Planning
    open FsPlanning.Search
    open NabfAgentLogic.LogicLib
    open NabfAgentLogic.Common
    open NabfAgentLogic.Search

    [<TestFixture>]
    type BiconnectedTest() = 
            
            //           |
            //           V
            //     D--C--A--B--E
            //       

            [<Test>]
            member this.Find_OneArticulationVertex_TwoSubGraphs() =
                let initialGraph =  [ ("a", { Identifier = "a"; Value = None; Edges = [(Some 1, "b");(Some 1, "c")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = None; Edges = [(Some 1, "a");(Some 1, "e")] |> Set.ofList })
                                    ; ("c", { Identifier = "c"; Value = None; Edges = [(Some 1, "d");(Some 1, "a")] |> Set.ofList })
                                    ; ("d", { Identifier = "d"; Value = None; Edges = [(Some 1, "c")] |> Set.ofList })
                                    ; ("e", { Identifier = "e"; Value = None; Edges = [(Some 1, "b")] |> Set.ofList })
                                    ] |> Map.ofList
                let articulationPoint = "a"
                let subgraph = Biconnected.find articulationPoint initialGraph
                Assert.AreEqual(Set [Set ["b";"e"]; Set ["c";"d"] ], Set subgraph)
                ()

            //           |
            //           V
            //     D--C--A--B--E
            //     |           |
            //     -------------  
            [<Test>]
            member this.Find_ZeroArticulationVertex_OneSubGraphs() =
                let initialGraph =  [ ("a", { Identifier = "a"; Value = None; Edges = [(Some 1, "b");(Some 1, "c")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = None; Edges = [(Some 1, "a");(Some 1, "e")] |> Set.ofList })
                                    ; ("c", { Identifier = "c"; Value = None; Edges = [(Some 1, "d");(Some 1, "a")] |> Set.ofList })
                                    ; ("d", { Identifier = "d"; Value = None; Edges = [(Some 1, "c");(Some 1, "e")] |> Set.ofList })
                                    ; ("e", { Identifier = "e"; Value = None; Edges = [(Some 1, "b");(Some 1, "d")] |> Set.ofList })
                                    ] |> Map.ofList
                let articulationPoint = "a"
                let subgraph = Biconnected.find articulationPoint initialGraph
                Assert.AreEqual(Set [Set ["b";"e";"c";"d"] ], Set subgraph)
                ()