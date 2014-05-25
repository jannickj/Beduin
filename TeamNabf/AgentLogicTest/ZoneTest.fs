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
            member this.FindZone_TwoNodes_LocateZone() =
                let initialGraph =  [ ("a", { Identifier = "a"; Value = Some 10; Edges = [(None, "b")] |> Set.ofList }) 
                                    ; ("b", { Identifier = "b"; Value = None; Edges = [(None, "a")] |> Set.ofList })
                                    ] |> Map.ofList
                let state = buildState "a" Explorer initialGraph
                let (Some (_,_,goals)) = findNewZone state
                let plan = makePlan state goals
                Assert.IsTrue (plan.IsSome)
                ()
