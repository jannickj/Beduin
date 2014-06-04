namespace AgentLogicTest
module PlanningTest =
    open System
    open NUnit.Framework
    open Graphing.Graph
    open NabfAgentLogic.AgentTypes
    open StructureBuilder
    open NabfAgentLogic.Explorer
    open NabfAgentLogic.Planning
    open NabfAgentLogic.ActionSpecifications
    open FsPlanning.Search

    [<TestFixture>]
    type RepairTest() = 

        [<Test>]
        member this.Repair_TwoMovesNoEnergyForSecond_RepairsInTheMiddle() =
            let initialGraph =  [ ("a", { Identifier = "a"; Value = Some 10; Edges = [(Some 9, "b")] |> Set.ofList }) 
                                ; ("b", { Identifier = "b"; Value = None; Edges = [(Some 9, "c")] |> Set.ofList })
                                ; ("c", { Identifier = "c"; Value = None; Edges = [] |> Set.ofList })
                                ] |> Map.ofList
            let state = buildStateWithEnergy "a" Explorer initialGraph 2

            let plan = [moveAction "b";moveAction "c"]
            let newPlan = repairPlanHelper state plan
            Assert.AreEqual((Some [rechargeAction;moveAction "b";rechargeAction;moveAction "c"]),newPlan)
            ()