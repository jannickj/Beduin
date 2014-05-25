namespace AgentLogicTest
module ZoneTest =
    open System
    open NUnit.Framework
    open Graphing.Graph
    open NabfAgentLogic.AgentTypes
    open StructureBuilder
    open NabfAgentLogic.Explorer
    open NabfAgentLogic.Planning

    [<TestFixture>]
    type ZoneTest() = 

            [<Test>]
            member this.FindAgentPositions_7NodeGraphWithIsland_Return3Positons() =
                let zone = [ { Identifier = "a"; Value = None; Edges = [(None, "b");(None, "c")] |> Set.ofList } 
                                    ;{ Identifier = "b"; Value = None; Edges = [(None, "a");(None, "c");(None, "d")] |> Set.ofList }
                                    ;{ Identifier = "c"; Value = None; Edges = [(None, "a");(None, "b");(None, "e")] |> Set.ofList }
                                    ;{ Identifier = "d"; Value = None; Edges = [(None, "b");(None, "e");(None, "f")] |> Set.ofList }
                                    ;{ Identifier = "e"; Value = None; Edges = [(None, "c");(None, "d");(None, "f");(None, "g")] |> Set.ofList }
                                    ;{ Identifier = "f"; Value = None; Edges = [(None, "e");(None, "d")] |> Set.ofList }
                                    ;{ Identifier = "g"; Value = None; Edges = [(None, "e")] |> Set.ofList }
                                    ]
                let initialGraph =  [ ("a", { Identifier = "a"; Value = None; Edges = [(None, "b");(None, "c")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = None; Edges = [(None, "a");(None, "c");(None, "d")] |> Set.ofList })
                                    ; ("c", { Identifier = "c"; Value = None; Edges = [(None, "a");(None, "b");(None, "e")] |> Set.ofList })
                                    ; ("d", { Identifier = "d"; Value = None; Edges = [(None, "b");(None, "e");(None, "f")] |> Set.ofList })
                                    ; ("e", { Identifier = "e"; Value = None; Edges = [(None, "c");(None, "d");(None, "f");(None, "g")] |> Set.ofList })
                                    ; ("f", { Identifier = "f"; Value = None; Edges = [(None, "e");(None, "d")] |> Set.ofList })
                                    ; ("g", { Identifier = "g"; Value = None; Edges = [(None, "e")] |> Set.ofList })
                                    ] |> Map.ofList
                let placement = findAgentPlacement zone initialGraph
                Assert.AreEqual(3,List.length placement)
                ()
            
            [<Test>]
            member this.FindAgentPositions_7NodeGraphWithoutIsland_Return3Positons() =
                let zone = [ { Identifier = "a"; Value = None; Edges = [(None, "b");(None, "c")] |> Set.ofList } 
                                    ; { Identifier = "b"; Value = None; Edges = [(None, "a");(None, "c");(None, "d")] |> Set.ofList }
                                    ; { Identifier = "c"; Value = None; Edges = [(None, "a");(None, "b");(None, "e")] |> Set.ofList }
                                    ; { Identifier = "d"; Value = None; Edges = [(None, "b");(None, "e");(None, "f")] |> Set.ofList }
                                    ; { Identifier = "e"; Value = None; Edges = [(None, "c");(None, "d");(None, "f");(None, "g")] |> Set.ofList }
                                    ; { Identifier = "f"; Value = None; Edges = [(None, "e");(None, "d")] |> Set.ofList }
                                    ; { Identifier = "g"; Value = None; Edges = [(None, "e");(None, "x")] |> Set.ofList }
                                    ]
                let initialGraph =  [ ("a", { Identifier = "a"; Value = None; Edges = [(None, "b");(None, "c")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = None; Edges = [(None, "a");(None, "c");(None, "d")] |> Set.ofList })
                                    ; ("c", { Identifier = "c"; Value = None; Edges = [(None, "a");(None, "b");(None, "e")] |> Set.ofList })
                                    ; ("d", { Identifier = "d"; Value = None; Edges = [(None, "b");(None, "e");(None, "f")] |> Set.ofList })
                                    ; ("e", { Identifier = "e"; Value = None; Edges = [(None, "c");(None, "d");(None, "f");(None, "g")] |> Set.ofList })
                                    ; ("f", { Identifier = "f"; Value = None; Edges = [(None, "e");(None, "d")] |> Set.ofList })
                                    ; ("g", { Identifier = "g"; Value = None; Edges = [(None, "e");(None, "x")] |> Set.ofList })
                                    ] |> Map.ofList
                let placement = findAgentPlacement zone initialGraph
                Assert.AreEqual(4,List.length placement)
                ()
           
            [<Test>]
            member this.FindAgentPositions_1NodeGraph_Return1Positons() =
                let zone = [{ Identifier = "v23"; Value = Some 10; Edges = [(Some 8, "v16")] |> Set.ofList }]
                let initialGraph =  [ ("v23", { Identifier = "v23"; Value = Some 10; Edges = [(Some 8, "v16")] |> Set.ofList }) 
                                    ] |> Map.ofList
                let placement = findAgentPlacement zone initialGraph
                Assert.AreEqual(1,List.length placement)
                ()

            [<Test>]
            member this.FindZone_TwoNodes_LocateZone() =
                let initialGraph =  [ ("a", { Identifier = "a"; Value = Some 10; Edges = [(None, "b")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = None; Edges = [(None, "a")] |> Set.ofList })
                                    ] |> Map.ofList
                let state = buildState "a" Explorer initialGraph
                let (Some (_,_,goals)) = findNewZone state
                let plan = makePlan state goals
                Assert.IsTrue (plan.IsSome)
                ()
