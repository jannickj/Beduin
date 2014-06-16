namespace AgentLogicTest
module CommonTest =
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

    [<TestFixture>]
    type CommonTest() = 

            [<Test>]
            member this.UnexploredTest_UnexploredNode_NearestVertexSatisfyingFindsIt() =
                let initialGraph =  [ ("a", { Identifier = "a"; Value = Some 10; Edges = [(Some 1, "b");(Some 1, "c")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = None; Edges = [(Some 1, "a");(None,"c")] |> Set.ofList })
                                    ; ("c", { Identifier = "c"; Value = None; Edges = [(None, "b")] |> Set.ofList })
                                    ] |> Map.ofList
                let state = StructureBuilder.buildState "a" Explorer initialGraph
                let var = nearestVertexSatisfying state isUnexplored
                Assert.AreEqual(var,Some "c")
                ()

            [<Test>]
            member this.UnexploredTest_ExploredNode_() =
                let initialGraph =  [ ("a", { Identifier = "a"; Value = Some 10; Edges = [(Some 1, "b");(Some 1, "c")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = None; Edges = [(Some 1, "a");(Some 1,"c")] |> Set.ofList })
                                    ; ("c", { Identifier = "c"; Value = None; Edges = [(Some 1, "b")] |> Set.ofList })
                                    ] |> Map.ofList
                let state = StructureBuilder.buildState "a" Explorer initialGraph
                let var = nearestVertexSatisfying state isUnexplored
                Assert.AreEqual(var,None)
                ()