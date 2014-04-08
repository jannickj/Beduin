module ExploreTest
    open System
    open NUnit.Framework
    open NabfAgentLogic.AgentLogic
    open Graphing.Graph
    open NabfAgentLogic.AgentTypes
    open NabfAgentLogic.AgentLogicLib
    open NabfAgentLogic.ExplorerLogic

    [<TestFixture>]
    type ExploreTest() = 

            [<Test>]
            member this.FindAgentPositions_7NodeGraphWithIsland_Return3Positons() =
                let initialGraph =  [ ("a", { Identifier = "a"; Value = None; Edges = [(None, "b");(None, "c")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = None; Edges = [(None, "a");(None, "c");(None, "d")] |> Set.ofList })
                                    ; ("c", { Identifier = "c"; Value = None; Edges = [(None, "a");(None, "b");(None, "e")] |> Set.ofList })
                                    ; ("d", { Identifier = "d"; Value = None; Edges = [(None, "b");(None, "e");(None, "f")] |> Set.ofList })
                                    ; ("e", { Identifier = "e"; Value = None; Edges = [(None, "c");(None, "d");(None, "f");(None, "g")] |> Set.ofList })
                                    ; ("f", { Identifier = "f"; Value = None; Edges = [(None, "e");(None, "d")] |> Set.ofList })
                                    ; ("g", { Identifier = "g"; Value = None; Edges = [(None, "e")] |> Set.ofList })
                                    ] |> Map.ofList
                let placement = findAgentPlacement initialGraph
                Assert.AreEqual(3,List.length placement)
                ()
            
            [<Test>]
            member this.FindAgentPositions_7NodeGraphWithoutIsland_Return3Positons() =
                let initialGraph =  [ ("a", { Identifier = "a"; Value = None; Edges = [(None, "b");(None, "c")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = None; Edges = [(None, "a");(None, "c");(None, "d")] |> Set.ofList })
                                    ; ("c", { Identifier = "c"; Value = None; Edges = [(None, "a");(None, "b");(None, "e")] |> Set.ofList })
                                    ; ("d", { Identifier = "d"; Value = None; Edges = [(None, "b");(None, "e");(None, "f")] |> Set.ofList })
                                    ; ("e", { Identifier = "e"; Value = None; Edges = [(None, "c");(None, "d");(None, "f");(None, "g")] |> Set.ofList })
                                    ; ("f", { Identifier = "f"; Value = None; Edges = [(None, "e");(None, "d")] |> Set.ofList })
                                    ; ("g", { Identifier = "g"; Value = None; Edges = [(None, "e");(None, "x")] |> Set.ofList })
                                    ] |> Map.ofList
                let placement = findAgentPlacement initialGraph
                Assert.AreEqual(4,List.length placement)
                ()
           
            [<Test>]
            member this.FindAgentPositions_1NodeGraph_Return1Positons() =
                let initialGraph =  [ ("v23", { Identifier = "v23"; Value = Some 10; Edges = [(Some 8, "v16")] |> Set.ofList }) 
                                    ] |> Map.ofList
                let placement = findAgentPlacement initialGraph
                Assert.AreEqual(1,List.length placement)
                ()