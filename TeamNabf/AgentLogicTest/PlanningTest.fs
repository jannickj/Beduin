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
    open NabfAgentLogic.Search.HeuristicDijkstra

    [<TestFixture>]
    type RepairTest() =

        [<Test>]
        member self.RepairPlan_EnemyAgentMovingFromBToC_PlanToCInsteadOfB() =
            let graph = 
                [ ("a", {Identifier = "a"; Value = None; Edges = [(None, "b"); (None, "c")] |> Set.ofList})
                ; ("b", {Identifier = "b"; Value = None; Edges = [(None, "a"); (None, "c")] |> Set.ofList})
                ; ("c", {Identifier = "c"; Value = None; Edges = [(None, "a"); (None, "b")] |> Set.ofList})
                ] |> Map.ofList

            let enemyName = "enemy"
            let enemy = buildEnemy enemyName "b"
            
            let stateNoHeuristics = { buildState "a" Saboteur graph with EnemyData = [enemy] }
            let state = List.fold updateHeuristic stateNoHeuristics ["a"; "b"; "c"]

            let intention = 
                ( "attack agent " + enemyName
                , Activity
                , [Requirement (Attacked enemyName)] 
                )

            let plan = formulatePlan state intention

            Assert.IsTrue (Option.isSome plan)

            let enemy' = { enemy with Node = "c" }
            let state' = { state with EnemyData = [enemy'] }

            let plan' = repairPlan state' intention plan.Value
            let actualPath = [for action in fst plan'.Value -> action.ActionType]

            let expectedPath = [Perform <| Goto "c"; Perform <| Attack "enemy"]

            printfn "expected path: %A" expectedPath
            printfn "actual path: %A" actualPath

            let assertion = expectedPath = actualPath
            Assert.IsTrue(assertion)


//        [<Test>]
//        member this.Repair_TwoMovesNoEnergyForSecond_RepairsInTheMiddle() =
////            let initialGraph =  [ ("a", { Identifier = "a"; Value = Some 10; Edges = [(Some 9, "b")] |> Set.ofList }) 
////                                ; ("b", { Identifier = "b"; Value = None; Edges = [(Some 9, "c")] |> Set.ofList })
////                                ; ("c", { Identifier = "c"; Value = None; Edges = [] |> Set.ofList })
////                                ] |> Map.ofList
////            let state = buildStateWithEnergy "a" Explorer initialGraph 2
////
////            let plan = [moveAction "b";moveAction "c"]
//////            let newPlan = repairPlanHelper state plan
////            Assert.AreEqual((Some [rechargeAction;moveAction "b";rechargeAction;moveAction "c"]),newPlan)
//            ()