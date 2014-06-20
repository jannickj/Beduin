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
    open NabfAgentLogic.Inspector
    open NabfAgentLogic.Common

    [<TestFixture>]
    type IntentionBuildingTests () =
//    let repairJob =
//            match inputState.MyJobs with
//            | (id,_)::_ -> Some (snd <| getJobFromJobID inputState id)
//            | _ -> None
//
//        match Map.tryFind MyRepairer inputState.Relations, inputState.Self.Status, repairJob with
//        | Some aName, Disabled, Some (RepairJob(_,rName)) when aName = rName && myRankIsGreatest inputState.Self.Name [rName] -> 
//            None
//        | Some aName,Disabled,_ ->
//            normalIntention 
        [<Test>]
        member self.FormulatePlanCommon_IntentToRechargeIfDisabled_PlanToRechargeIfDisabled () =     
            //repairer and repairee is on same node     
            let world = 
                [ ("a", {Identifier = "a"; Value = None; Edges = [(None, "b")] |> Set.ofList})
                ; ("b", {Identifier = "b"; Value = None; Edges = [(None, "a")] |> Set.ofList})
                ] |> Map.ofList  
            let state = buildState "a" Inspector world
            let stateAsDisabled = {state with Self = {state.Self with Status = EntityStatus.Disabled} }
            let stateWithRelations = {stateAsDisabled with Relations = Map.add MyRepairer "carlos" stateAsDisabled.Relations }
            let stateWithCarlos = {stateWithRelations with FriendlyData = [(buildAgentWithRole "carlos" "Team Love Unit testing" "a" (Some AgentRole.Repairer))] }

            let intention = getRepaired stateWithCarlos                                
            let intentionTuple = (intention.Value.Label, intention.Value.Type, intention.Value.Objectives)

            let expectedPlan = [rechargeAction]

            let actualPlan = formulatePlan stateWithCarlos intentionTuple

            let assertion = 
                match actualPlan with
                | Some (plan, _) -> plan = expectedPlan
                | None -> false

            Assert.IsTrue (assertion)
        
        [<Test>]
        member self.FormulatePlanInspector_IntentToInspectVertex_PlanToInspectVertex () =
        (*
         *  a --- b
         *  
         *  Inspector at node 'a'. Enemy with unknown role at 'b'. Plan to move to 'b' and inspect.
         *
         *)

            let world = 
                [ ("a", {Identifier = "a"; Value = None; Edges = [(None, "b")] |> Set.ofList})
                ; ("b", {Identifier = "b"; Value = None; Edges = [(None, "a")] |> Set.ofList})
                ] |> Map.ofList
            
            let enemy = buildEnemy "enemy" "b"
            let state = { buildState "a" Inspector world with EnemyData = [enemy] }

            let intention = spontanousInspectAgent state
            let intentionTuple = (intention.Value.Label, intention.Value.Type, intention.Value.Objectives)

            let expectedPlan = [skipAction; moveAction "b"; inspectAction None]

            let actualPlan = formulatePlan state intentionTuple

            let assertion = 
                match actualPlan with
                | Some (plan, _) -> plan = expectedPlan
                | None -> false

            Assert.IsTrue (assertion)

        [<Test>]
        member self.SpontaneoulsyInspectVertex_TwoNeighbourVerticesWithAgents_IntentToInspectVertexWithMostUnknownEnemies () =
            (*
             *  a -- b
             *   \  /
             *    \/
             *    c
             *  
             *  Inspector at 'a'. Two unknown enemies at 'b'. Two known and one unknown enemy at 'c'.
             *  We want to inspect 'c'.
             *
             *)
            let world = 
                [ ("a", {Identifier = "a"; Value = None; Edges = [(None, "b"); (None, "c")] |> Set.ofList})
                ; ("b", {Identifier = "b"; Value = None; Edges = [(None, "a")] |> Set.ofList})
                ; ("c", {Identifier = "c"; Value = None; Edges = [(None, "a")] |> Set.ofList})
                ] |> Map.ofList

            let enemy1 = buildEnemyWithRole "enemy1" "b" (Some Explorer)
            let knownRole = Some Explorer
            let enemyData = 
                [ buildEnemyWithRole "enemy1" "b" None
                ; buildEnemyWithRole "enemy2" "b" None
                ; buildEnemyWithRole "enemy3" "c" knownRole
                ; buildEnemyWithRole "enemy4" "c" knownRole
                ; buildEnemyWithRole "enemy5" "c" None
                ]

            let state = 
                { buildState "a" Inspector world with EnemyData = enemyData }

            let intention = spontanousInspectAgent state

            let actual = 
                match intention with
                | Some intent -> Some <| List.collect (fun obj -> goalList obj state) intent.Objectives
                | None -> None
            
            let expected = Some <| [Inspected "b"]
            let assertion = expected = actual

            Assert.IsTrue (assertion)

    [<TestFixture>]
    type RepairTest() =

        [<Test>]
        member self.RepairPlanNotEnoughEnergy_LackingEnergyForFirstAction_PrependRechargeAction () =
            let world = [("a", {Identifier = "a"; Value = None; Edges = Set.empty})] |> Map.ofList
            let state = buildStateWithEnergy "a" Explorer world 0
            let plan = [skipAction; probeAction None]

            let objectives = [Requirement <| Probed "a"]
            let intention = ("", Activity, objectives)

            let expectedPlan = [rechargeAction; probeAction None]

            let actualPlan = repairPlan state intention (plan, objectives)
            let assertion = (fst actualPlan.Value) = expectedPlan
            Assert.IsTrue (assertion)

        [<Test>] 
        member self.RepairPlan_LastActionFailedRandomly_TryLastActionAgain () =
            let world = [("a", {Identifier = "a"; Value = None; Edges = Set.empty})] |> Map.ofList
            let state = { buildStateWithEnergy "a" Explorer world 30 with LastActionResult = FailedRandom }
            let plan = [probeAction None]

            let objectives = [Requirement <| Probed "a"]
            let intention = ("", Activity, objectives)

            let expectedPlan = plan
            let actualPlan = repairPlan state intention (plan, objectives)

            let assertion = (fst actualPlan.Value) = expectedPlan
            Assert.IsTrue (assertion)

        [<Test>]
        member self.RepairPlan_LastActionFailedRandomlyAndUsedAllEnergy_TryLastActionAgainAndPrependRechargeAction () =
            let world = [("a", {Identifier = "a"; Value = None; Edges = Set.empty})] |> Map.ofList
            let state = { buildStateWithEnergy "a" Explorer world 0 with LastActionResult = FailedRandom }
            let plan = [probeAction None]

            let objectives = [Requirement <| Probed "a"]
            let intention = ("", Activity, objectives)

            let expectedPlan = [rechargeAction; probeAction None]
            let actualPlan = repairPlan state intention (plan, objectives)

            let assertion = (fst actualPlan.Value) = expectedPlan
            Assert.IsTrue (assertion)


        [<Test>]
        member self.RepairPlanAttackEnemy_EnemyMovingFromBToC_PlanToCInsteadOfB() =
        (*
         *  a -- b
         *   \  /
         *    \/
         *    c
         *
         * The saboteur starts in 'a', and has planned to attack the enemy agent at 'b'.
         * The enemy moves to 'c', and the repaired plan is to shortcut to 'c' and attack.
         *
         *)
            let graph = 
                [ ("a", {Identifier = "a"; Value = None; Edges = [(None, "b"); (None, "c")] |> Set.ofList})
                ; ("b", {Identifier = "b"; Value = None; Edges = [(None, "a"); (None, "c")] |> Set.ofList})
                ; ("c", {Identifier = "c"; Value = None; Edges = [(None, "a"); (None, "b")] |> Set.ofList})
                ] |> Map.ofList

            let enemyName = "enemy"
            let enemy = buildEnemy enemyName "b"
            
            let state = 
                { buildState "a" Saboteur graph with EnemyData = [enemy] }
                |> enhanceStateWithGraphHeuristics

            let intention = 
                ( "attack agent " + enemyName
                , Activity
                , [Requirement (Attacked enemyName)] 
                )

            let plan = formulatePlan state intention

            let enemy' = { enemy with Node = "c" }
            let state' = { state with EnemyData = [enemy'] }

            let plan' = repairPlan state' intention plan.Value
            let actualPath = [for action in fst plan'.Value -> action.ActionType]

            let expectedPath = [Perform <| Goto "c"; Perform <| Attack "enemy"]

            let assertion = expectedPath = actualPath
            Assert.IsTrue(assertion)

        [<Test>] 
        member self.RepairPlanProbeZone_OneVertexInZoneIsProbed_PlanToProbeOtherNodeOnly() =
        (*
         *  a -- b
         *   \  /
         *    \/
         *    c
         *
         * The agent starts in 'a', planning to probe 'b' and 'c'. 
         * Another agent probes 'b', the repaired plan is to probe 'c' only.
         *
         *)
            let world = 
                [ ("a", {Identifier = "a"; Value = Some 10; Edges = [(None, "b"); (None, "c")] |> Set.ofList})
                ; ("b", {Identifier = "b"; Value = Some 1; Edges = [(None, "a"); (None, "c")] |> Set.ofList})
                ; ("c", {Identifier = "c"; Value = None; Edges = [(None, "a"); (None, "b")] |> Set.ofList})
                ] |> Map.ofList 
            
            let originalPlan = 
                [ skipAction
                ; moveAction "b"
                ; probeAction None
                ; moveAction "c"
                ; probeAction None
                ]

            let objective = MultiGoal (fun _ -> [Probed "c"])

            let state = 
                buildState "a" Explorer world 
                |> enhanceStateWithGraphHeuristics

            let intention = ("probe zone", Activity, [objective])

            let expectedPlan = 
                [ moveAction "c"
                ; probeAction None
                ]

            let actualPlan = repairPlan state intention (originalPlan, [objective])

            let assertion = expectedPlan = (fst actualPlan.Value)

            Assert.IsTrue (assertion)

        [<Test>]
        member self.RepairPlanProbeZone_VertexInPathIsProbed_RemoveProbeAction() =
        (*        
         *        a      
         *        |       
         *        b       
         *        |       
         *        c
         *        |
         *        d
         *  
         *      The explorer has planned to probe 'b', 'c', and 'd'. Another explorer probes 'b'.
         *      The explorer repairs the plan, but keeps the first part of the plan, without probing 'b'.
         *)

            let world = 
                [ ("a", {Identifier = "a"; Value = Some 10; Edges = [(None, "b")] |> Set.ofList})
                ; ("b", {Identifier = "b"; Value = Some 1; Edges = [(None, "a"); (None, "c")] |> Set.ofList})
                ; ("c", {Identifier = "c"; Value = None; Edges = [(None, "b"); (None, "d")] |> Set.ofList})
                ; ("d", {Identifier = "d"; Value = None; Edges = [(None, "c")] |> Set.ofList})
                ] |> Map.ofList 

            let state = 
                buildState "a" Explorer world
                |> enhanceStateWithGraphHeuristics 
                         
            let originalPlan = 
                [ skipAction
                ; moveAction "b"; probeAction None
                ; moveAction "c"; probeAction None
                ; moveAction "d"; probeAction None
                ]

            let objective = MultiGoal (fun _ -> [Probed "c"; Probed "d"])
            let intention = ("probe zone", Activity, [objective])

            let expectedPlan =
                [ moveAction "b"
                ; moveAction "c"; probeAction None
                ; moveAction "d"; probeAction None
                ]

            let actualPlan = repairPlan state (Some intention) (originalPlan, [objective])
            let assertion = (fst actualPlan.Value) = expectedPlan

            Assert.IsTrue(assertion)

        [<Test>]
        member self.RepairPlanProbeZone_VertexInPathIsProbed_ShortCutPlan() =
        (*
         *       c
         *      / \
         *     1   1
         *    /     \
         *   b --9-- d
         *    \     /
         *     1   9
         *      \ /
         *       a
         *
         * The explorer starts in 'a', and has planned to probe 'b', 'c', and 'd', in order.
         * 'c'is explored by another agent. The repaired plan is to probe 'b' and 'd' in order
         *
         *)

            let world = 
                [ ("a", {Identifier = "a"; Value = Some 10; Edges = [(Some 1, "b"); (Some 9, "d")] |> Set.ofList})
                ; ("b", {Identifier = "b"; Value = None; Edges = [(Some 1, "a"); (Some 1, "c"); (Some 9, "d")] |> Set.ofList})
                ; ("c", {Identifier = "c"; Value = Some 1; Edges = [(Some 1, "b"); (Some 1, "d")] |> Set.ofList})
                ; ("d", {Identifier = "d"; Value = None; Edges = [(Some 9, "a"); (Some 9, "b"); (Some 1, "c")] |> Set.ofList})
                ] |> Map.ofList 

            let state = 
                buildState "a" Explorer world
                |> enhanceStateWithGraphHeuristics

            let originalPlan =
                [ skipAction
                ; moveAction "b"; probeAction None
                ; moveAction "c"; probeAction None
                ; moveAction "d"; probeAction None
                ]

            let objective = MultiGoal (fun _ -> [Probed "b"; Probed "d"])
            let intention = ("probe zone", Activity, [objective])

            let expectedPlan =
                [ moveAction "b"; probeAction None
                ; moveAction "d"; probeAction None
                ]
            
            let actualPlan = repairPlan state (Some intention) (originalPlan, [objective])
            let assertion = fst actualPlan.Value = expectedPlan

            Assert.IsTrue (assertion)

//        [<Test>]
//        member self.FormulatePlanRepairAgent_DisabledAgentTwoEdgesAway_GoThereAndRepair() =
//            let world = 
//                [ ("a", {Identifier = "a"; Value = Some 10; Edges = [(Some 1, "b"); (Some 9, "d")] |> Set.ofList})
//                ; ("b", {Identifier = "b"; Value = None; Edges = [(Some 1, "a"); (Some 1, "c"); (Some 9, "d")] |> Set.ofList})
//                ; ("c", {Identifier = "c"; Value = Some 1; Edges = [(Some 1, "b"); (Some 1, "d")] |> Set.ofList})
//                ; ("d", {Identifier = "d"; Value = None; Edges = [(Some 9, "a"); (Some 9, "b"); (Some 1, "c")] |> Set.ofList})
//                ] |> Map.ofList 
//
//            let friend = 
//            let friendlyData = [ { buildAgentWithRole "friend" "Team Love Unit testing" "c" with Self = ]
//
//            let state =
//                { buildState "a" Repairer world with FriendlyData = friendlyData }

            
