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
    open NabfAgentLogic.GeneralLib
    open NabfAgentLogic.LogicLib

    [<TestFixture>]
    type IntentionBuildingTests () =

        [<Test>]
        member self.FormulatePlanCommon_IntentToRechargeIfDisabled_PlanToRechargeIfDisabled_SameNode () =     
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
            let intentionTuple =normalIntention (intention.Value.Label, intention.Value.Type, intention.Value.Objectives)


            let actualPlan = formulatePlan stateWithCarlos intentionTuple
            let realPlan = repairPlan stateWithCarlos "" actualPlan.Value
            let myNextAction = nextAction stateWithCarlos intention.Value realPlan.Value


            match myNextAction with
            | Some (action,_) -> Assert.AreEqual(Perform Recharge, action)
            

        [<Test>]
        member self.FormulatePlanCommon_IntentToRechargeIfDisabled_PlanToRechargeIfDisabled_NotSameNode () =     
            //repairer and repairee is NOT on same node     
            let world = 
                [ ("a", {Identifier = "a"; Value = None; Edges = [(None, "b")] |> Set.ofList})
                ; ("b", {Identifier = "b"; Value = None; Edges = [(None, "a")] |> Set.ofList})
                ; ("c", {Identifier = "c"; Value = None; Edges = [(None, "b")] |> Set.ofList})
                ] |> Map.ofList  
            let state = buildState "a" Inspector world
            let stateAsDisabled = {state with Self = {state.Self with Status = EntityStatus.Disabled} }
            let stateWithRelations = {stateAsDisabled with Relations = Map.add MyRepairer "carlos" stateAsDisabled.Relations }
            let stateWithCarlos = {stateWithRelations with FriendlyData = [(buildAgentWithRole "carlos" "Team Love Unit testing" "c" (Some AgentRole.Repairer))] }

            let intention = getRepaired stateWithCarlos                                
            let intentionTuple = normalIntention (intention.Value.Label, intention.Value.Type, intention.Value.Objectives)
            
            let actualPlan = formulatePlan stateWithCarlos intentionTuple
            let realPlan = repairPlan stateWithCarlos "" actualPlan.Value
            let myNextAction = nextAction stateWithCarlos intention.Value realPlan.Value


            match myNextAction with
            | Some (action,rem) -> Assert.AreEqual(Perform (Goto "b"), action)
        
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
            let intentionTuple =normalIntention (intention.Value.Label, intention.Value.Type, intention.Value.Objectives)

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

        [<Test>]
        member self.FormulatePlanRepairer_RepaireeAdjacentToRepairer_RepairerMovesThenRepairs () =
        (*
         * Repairer is at "a", repairee is at "b". Repairer should move to "b" and repair (no ranged repairing).
         *)
            let world = 
                [ ("a", {Identifier = "a"; Value = None; Edges = set [(None, "b")]})
                ; ("b", {Identifier = "b"; Value = None; Edges = set [(None, "a")]})
                ] |> Map.ofList

            let friendName = "Friend"
            let repairerAt = "a"
            let friendAt = "b"
            let state = { buildState repairerAt Repairer world with FriendlyData = [buildFriend friendName friendAt] }

            let intention = normalIntention <| ("test", Activity, [Requirement <| Repaired friendName])
            let actualPlan = formulatePlan state intention

            let expectedPlan = [Perform (Goto friendAt); Perform (Repair friendName)]

            let assertion = 
                match actualPlan with
                | Some (expectedPlan, _) -> true
                | _ -> false

            Assert.IsTrue (assertion)

    [<TestFixture>]
    type RepairTest() =

        [<Test>]
        member self.RepairPlanNotEnoughEnergy_LackingEnergyForFirstAction_PrependRechargeAction () =
            let world = [("a", {Identifier = "a"; Value = None; Edges = Set.empty})] |> Map.ofList
            let state = buildStateWithEnergy "a" Explorer world 0
            let plan = [skipAction; probeAction None]

            let objectives = [Requirement <| Probed "a"]
//            let intention = ("", Activity, objectives)

            let expectedPlan = [rechargeAction; probeAction None]

            let actualPlan = repairPlan state "" (plan, objectives)
            let assertion = (fst actualPlan.Value) = expectedPlan
            Assert.IsTrue (assertion)

        [<Test>] 
        member self.RepairPlan_LastActionFailedRandomly_TryLastActionAgain () =
            let world = [("a", {Identifier = "a"; Value = None; Edges = Set.empty})] |> Map.ofList
            let state = { buildStateWithEnergy "a" Explorer world 30 with LastActionResult = FailedRandom }
            let plan = [probeAction None]

            let objectives = [Requirement <| Probed "a"]
//            let intention = ("", Activity, objectives)

            let expectedPlan = plan
            let actualPlan = repairPlan state "" (plan, objectives)

            let assertion = (fst actualPlan.Value) = expectedPlan
            Assert.IsTrue (assertion)

        [<Test>]
        member self.RepairPlan_LastActionFailedRandomlyAndUsedAllEnergy_TryLastActionAgainAndPrependRechargeAction () =
            let world = [("a", {Identifier = "a"; Value = None; Edges = Set.empty})] |> Map.ofList
            let state = { buildStateWithEnergy "a" Explorer world 0 with LastActionResult = FailedRandom }
            let plan = [probeAction None]

            let objectives = [Requirement <| Probed "a"]
//            let intention = ("", Activity, objectives)

            let expectedPlan = [rechargeAction; probeAction None]
            let actualPlan = repairPlan state "" (plan, objectives)

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
                normalIntention
                    ( "attack agent " + enemyName
                    , Activity
                    , [Requirement (Attacked enemyName)] 
                    )
           
            
            let plan = formulatePlan state intention

            let enemy' = { enemy with Node = "c" }
            let state' = { state with EnemyData = [enemy'] }

            let plan' = repairPlan state' "" plan.Value
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
                ; probeAction (Some "b")
                ; probeAction (Some "c")
                ]

            let objective = MultiGoal (fun _ -> [Probed "c"])

            let state = 
                buildState "a" Explorer world 
                |> enhanceStateWithGraphHeuristics

//            let intention = ("probe zone", Activity, [objective])

            let expectedPlan = 
                [ probeAction (Some "c") ]

            let actualPlan = repairPlan state "" (originalPlan, [objective])

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
                ; moveAction "b"
                ; probeAction None
                ; moveAction "c"
                ; probeAction (Some "d")
                ; probeAction None
                ]

            let objective = MultiGoal (fun _ -> [Probed "c"; Probed "d"])
//            let intention = ("probe zone", Activity, [objective])

            let expectedPlan =
                [ moveAction "b"
                ; moveAction "c"
                ; probeAction (Some "d")
                ; probeAction None
                ]

            let actualPlan = repairPlan state "" (originalPlan, [objective])
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
                ; moveAction "b"
                ; probeAction (Some "c")
                ; probeAction (Some "d")
                ; probeAction None
                ]

            let objective = MultiGoal (fun _ -> [Probed "b"; Probed "d"])
//            let intention = ("probe zone", Activity, [objective])

            let expectedPlan =
                [ moveAction "b"
                ; probeAction (Some "d")
                ; probeAction None
                ]
            
            let actualPlan = repairPlan state "" (originalPlan, [objective])
            let assertion = fst actualPlan.Value = expectedPlan

            Assert.IsTrue (assertion)

        
        [<Test>] 
        member self.RepairPlan_MissingEnergy_PerformActionsThenDeclarePlanFinished() =
            let world = 
                [ ("a", {Identifier = "a"; Value = None; Edges = Set.empty })
                ] |> Map.ofList 

            let state = buildState "a" Explorer world
            let noEnergyState = { state with Self = { state.Self with Energy = Some 0 } }

            let intention = normalIntention("testIntention",Activity,[Plan (fun _ -> Some [Perform Parry])])
            let (Some plan) = formulatePlan noEnergyState intention
            let (Some updatedPlan) = repairPlan noEnergyState "" plan
            let (Some (action,remPlan)) = nextAction noEnergyState intention updatedPlan
            let (Some updatedPlan2) = repairPlan state "" remPlan
            let (Some (action2,remPlan2)) = nextAction noEnergyState intention updatedPlan2
            Assert.AreEqual(Perform Recharge, action)
            Assert.AreEqual(Perform Parry, action2)
            let finished = solutionFinished noEnergyState intention remPlan2
            Assert.True (finished)
            ()

        [<Test>] 
        member self.MultipleObjectives_TwoGotoGoals_SolveFirstThenTheOther() =
            let world = 
                [ ("a", {Identifier = "a"; Value = None; Edges = Set [(None, "b")] })
                ; ("b", {Identifier = "b"; Value = None; Edges = Set [(None, "a");(None, "c")]})
                ; ("c", {Identifier = "c"; Value = None; Edges = Set [(None, "b")]})
                ] |> Map.ofList 

            let state = 
                buildState "a" Explorer world
                |> enhanceStateWithGraphHeuristics
            let intention = normalIntention("testIntention",Activity,[Requirement(At "b"); Requirement(At "c")])
            
            let (Some plan) = formulatePlan state intention
            let (Some updatedPlan) = repairPlan state "" plan
            let (Some (action,remPlan)) = nextAction state intention updatedPlan
            
            Assert.AreEqual(Perform <| Goto "b",action)

            let updateState = { state with Self = {state.Self with Node = "b" }}
            let (Some updatedPlan2) = repairPlan updateState "" remPlan
            let (Some (action2,remPlan2)) = nextAction updateState intention updatedPlan2
            
            Assert.AreEqual(Perform <| Goto "c",action2)
            
            let updateState2 = { state with Self = {state.Self with Node = "c" }}
            let solFinished = solutionFinished updateState2 intention remPlan2
            Assert.IsTrue (solFinished)
            ()

        [<Test>] 
        member self.MultipleObjectives_CommunicateThenRepairGoals_SolveFirstThenTheOther() =
            let world = 
                [ ("a", {Identifier = "a"; Value = None; Edges = Set.empty })
                ] |> Map.ofList 

            let state = 
                buildState "a" Repairer world
                |> enhanceStateWithGraphHeuristics
            let state = {state with FriendlyData = [{ buildAgent "A1" state.Self.Team true with Node = "a"; Status=Disabled}]}
            let mail = (state.Self.Name,"A1",GoingToRepairYou)
            let intention = normalIntention("testIntention",Activity,[Plan(fun _ -> Some [Communicate <| SendMail mail]); Requirement (Repaired("A1"))])
            
            let (Some plan) = formulatePlan state intention
            let (Some updatedPlan) = repairPlan state "" plan
            let (Some (action,remPlan)) = nextAction state intention updatedPlan
            
            Assert.AreEqual(Communicate <| SendMail mail,action)

            let (Some updatedPlan2) = repairPlan state "" remPlan
            let (Some (action2,remPlan2)) = nextAction state intention updatedPlan2
            
            Assert.AreEqual(Perform <| Repair "A1",action2)
            ()

        [<Test>] 
        member self.MultipleObjectives_CommunicateThenMoveGoals_SolveFirstThenTheOther() =
            let world = 
                [ ("a", {Identifier = "a"; Value = None; Edges = set [(None, "b")] })
                ; ("b", {Identifier = "b"; Value = None; Edges = set [(None, "a")] })
                ] |> Map.ofList 

            let state = 
                buildState "a" Explorer world
                |> enhanceStateWithGraphHeuristics
            let state = {state with FriendlyData = [{ buildAgent "A1" state.Self.Team true with Node = "a"}]}
            let mail = (state.Self.Name,"A1",GoingToRepairYou)
            let intention = normalIntention 
                                ("testIntention"
                                , Activity
                                , [Plan(fun _ -> Some [Communicate <| SendMail mail]); Requirement (At "b")]
                                )
            
            let (Some plan) = formulatePlan state intention
            let (Some updatedPlan) = repairPlan state "" plan
            let (Some (action,remPlan)) = nextAction state intention updatedPlan
            
            Assert.AreEqual(Communicate <| SendMail mail,action)

            let (Some updatedPlan2) = repairPlan state "" remPlan
            let (Some (action2,remPlan2)) = nextAction state intention updatedPlan2
            
            Assert.AreEqual(Perform <| Goto "b", action2)
            ()


        [<Test>]
        member self.RepairPreLaidPlan_NotEnoughEnergyForPreLaidPlan_PrependRechargeAction () =
            let world = [ ("a", {Identifier = "a"; Value = None; Edges = set []}) ] |> Map.ofList
            let enemyData = [buildEnemyWithRole "enemy" "a" (Some Saboteur)]
            let state = { buildStateWithEnergy "a" Sentinel world 0 with EnemyData = enemyData }

            let originalPlan = [skipAction; parryAction]

            let actual = repairPlan state "" (originalPlan, [Plan (fun _ -> Some [])])

            let expected = 
                [ rechargeAction
                ; parryAction
                ]

            let assertion = expected = fst actual.Value

            Assert.IsTrue (assertion)

