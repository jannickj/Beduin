namespace AgentLogicTest
module HeuristicTest =
    open System
    open NUnit.Framework
    open Graphing.Graph
    open NabfAgentLogic.AgentTypes
    open StructureBuilder
    open NabfAgentLogic.Explorer
    open NabfAgentLogic.Planning
    open NabfAgentLogic.ActionSpecifications
    open NabfAgentLogic.Search.FloydWarshall
    open FsPlanning.Search

    [<TestFixture>]
    type HeuristicTest() = 

        [<Test>]
        member this.FloydWarshall_AddNewVertex_ShortestPathsCorrectlyFound() =

            let graph =         [ ("a", { Identifier = "a"; Value = None; Edges = [(Some 1, "b");(Some 2, "c")] |> Set.ofList }) 
                                ; ("b", { Identifier = "b"; Value = None; Edges = [(Some 1, "a");(Some 2, "c");(Some 4, "d")] |> Set.ofList })
                                ; ("c", { Identifier = "c"; Value = None; Edges = [(Some 2, "a");(Some 2, "b");(Some 2, "d")] |> Set.ofList })
                                ; ("d", { Identifier = "d"; Value = None; Edges = [(Some 4, "b");(Some 2, "c");(Some 2, "e")] |> Set.ofList })
                                ; ("e", { Identifier = "e"; Value = None; Edges = [(Some 2, "d")] |> Set.ofList })
                                ] |> Map.ofList

            let map = [(("b","c"),2);(("c","b"),2);(("c","c"),0);(("b","b"),0);(("d","d"),0);(("d","b"),4);(("b","d"),4);(("c","d"),2);(("d","c"),2);(("e","e"),0);(("e","d"),2);(("e","c"),4);(("e","b"),6);(("d","e"),2);(("c","e"),4);(("b","e"),6)] |> Map.ofList

            let v = floydWarshallList graph ([(("a","a"),0)] |> Map.ofList) ["a";"b";"c";"d";"e"]
            let v2 = floydWarshallComplete graph
            Assert.AreEqual(v,v2)
            ()