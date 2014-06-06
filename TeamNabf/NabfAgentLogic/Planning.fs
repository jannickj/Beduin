namespace NabfAgentLogic
module Planning =
    open ActionSpecifications
    open FsPlanning.Search.Problem
    open FsPlanning.Search.Astar
    open NabfAgentLogic.AgentTypes
    open Graphing.Graph
    open FsPlanning.Agent.Planning
    open Logging
    open Constants
    open GoalSpecifications
    open System.Diagnostics

    type Plan = (ActionSpecification list) * (Objective list)
    let test = 1
    let flip f x y = f y x
    let snd3 (_, a, _) = a 

    // Transforms objectives into a normalized form of State -> (Goal list)
    let goalFunc objective = 
        match objective with
        | MultiGoal func  -> func 
        | Requirement req -> fun _ -> [req]
        | Plan _          -> failwith "Trying to search on predefined plan"

    let goalList objective state = goalFunc objective <| state

    let wrappedGoalTest goalTest state = 
        try goalTest state with
        | ex -> logError <| sprintf "goal test: %A \nfailed with:\n %A" goalTest ex
                false

    let distance (goals : Goal list) state cost =
        let heuValue = List.max <| List.map (fun goal -> goalHeuristics goal <| state) goals 

        let minimumTraversalCost = (heuValue / EDGE_COST_MAX) * turnCost state
        let rechargesRequiredCost = (heuValue / state.Self.MaxEnergy.Value) * turnCost state
        heuValue + minimumTraversalCost + rechargesRequiredCost
      
    let satisfiedGoalCount goals state =
        List.length <| List.filter ((flip generateGoalCondition) state) goals

    let unSatisfiedGoalCount goals state =
        List.length <| List.filter (not << (flip generateGoalCondition) state) goals

    let h goals state cost = 
        (unSatisfiedGoalCount goals state, cost + distance goals state cost)
    
    let goalTest goals state = 
        List.forall (fun goal -> generateGoalCondition goal state) goals

    let agentProblem (state : State) goals = 
        let headGoal = List.head goals

        { InitialState = state
        ; GoalTest     = wrappedGoalTest <| goalTest goals
        ; Actions      = fun state -> List.filter (isApplicable state) (availableActions headGoal state)
        ; Result       = fun state action -> action.Effect state
        ; StepCost     = fun state action -> action.Cost state
        ; Heuristic    = h goals
        }

    let rec prunePlanHelper path goals =
        let gC searchnode = unSatisfiedGoalCount goals searchnode.State
        match path with
        | searchNode1 :: searchNode2 :: tail when gC searchNode1 < gC searchNode2 ->
            List.rev <| searchNode2 :: tail
        | sn1 :: sn2 :: tail -> prunePlanHelper (sn2 :: tail) goals
        | [sn] -> [sn]
        | [] -> []

    let prunePlan plan goals = 
        match plan with
        | Some {Cost = _; Path = path} -> Some <| prunePlanHelper (List.rev path) goals
        | None -> None

    let makePlan initstate objectives =
        let state = { initstate with LastAction = Skip }
        match objectives with
        | (Plan plan) :: _ -> 
            match plan state with
            | Some p -> Some (List.map actionSpecification <| p, objectives)
            | None -> None
        | goalObjective :: _ -> 
            let goals = goalList goalObjective state
            let stopwatch = System.Diagnostics.Stopwatch.StartNew()
            let breakTest (stopwatch : Stopwatch) = stopwatch.ElapsedMilliseconds > Constants.MAX_PLANNING_TIME_MS

            let plan = solveSearchNodePath aStar (agentProblem state goals) (fun () -> breakTest stopwatch)
            let prunedPlan = prunePlan plan goals
            match prunedPlan with 
            | Some path when not <| List.forall (fun node -> satisfiedGoalCount (goalList goalObjective node.State) node.State = 0) path -> 
                let actions = List.map (fun node -> node.Action.Value) path
                logImportant (sprintf "Found plan: %A" <| List.map (fun action -> action.ActionType) actions)
                Some (actions, objectives)
            | _ -> None
        | [] -> Some ([], objectives)

       
    let formulatePlan (state : State) intent = 
        let (name, inttype, goals) = intent
        match inttype with
        | Communication -> 
            logInfo ("Sending message " + name)
        | Activity ->
            logImportant ("Planning to " + name)
        | _ -> ()
        makePlan state goals

//    let rec repairPlanHelper state plan = 
//
//        let restPlan state action actionList =
//            let restOfPlan = repairPlanHelper state actionList
//            match restOfPlan with
//            | Some plan -> Some <| action :: plan
//            | None -> None
//
//        match plan with
//        | action :: tail when isApplicable state action ->
//            restPlan (action.Effect state) action tail
//        | action :: tail ->
//            logImportant <| sprintf "Inconsistency found! state does not satisfy %A" action.ActionType
//            logInfo <| sprintf "the following errors were found: %A" (unSatisfiedPreconditions state action)
//            let gluePlan = makePlan state ([Requirement <| ((flip isApplicable action), None)])
//
//            match gluePlan with
//            // If we find a non-empty glue plan [a; b; c], prepend it to the plan and continue recursing
//            | Some (newAction :: newTail, _) -> 
//                logInfo <| sprintf "Found glue plan %A" (List.map (fun action -> action.ActionType) (newAction :: newTail))
//                restPlan (newAction.Effect state) newAction (newTail @ action :: tail)
//      
//            | Some (_, _) -> restPlan (action.Effect state) action tail
//            | None -> 
//                logImportant <| sprintf "Failed to find glue plan" 
//                None
//        | _ -> Some plan

    let repairPlan state intent (plan : Plan) = 
        
        let rec helper state plan =
            match plan with
            | action :: tail when isApplicable state action ->
                match helper (action.Effect state) tail with
                | Some plan -> Some <| [action] @ plan
                | None -> None
            | action :: tail ->
                logImportant <| sprintf "Inconsistency found! state does not satisfy %A" action.ActionType
//                let gluePlan = makePlan state ([Requirement <| ((flip isApplicable action), None, CheckGoal)])
//                match gluePlan with
//                | Some (plan, _) ->
//                    logInfo <| sprintf "Found glue plan %A" (List.map (fun action -> action.ActionType) plan)
//                    Some <| plan @ (action :: tail)
//                | None -> 
//                    logImportant <| sprintf "Failed to find glue plan" 
//                    None
                Some []
            | [] -> Some []

        
        let (path, goals) = plan
        logInfo <| sprintf "Repairing plan %A" (List.map (fun action -> action.ActionType) path)
//        let newPlan = repairPlanHelper state path
        let newPlan = helper state path

        match newPlan with
        | Some p -> 
            logInfo <| sprintf "repaired plan: %A" (List.map (fun action -> action.ActionType) p)
            Some (p, goals)
        | None -> None

    let solutionFinished state intent solution = 
        match solution with
        | (_, [Plan plan]) -> 
            match plan state with
            | Some [] 
            | None -> true
            | _ -> false
        | (_, [objective]) -> (wrappedGoalTest <| goalTest (goalList objective state)) state
        | (_, []) -> true
        | _ -> false
    
    let rec nextAction state (intent : Intention) (plan : Plan) =
        match plan with
        | (action :: rest, goals) -> Some (action.ActionType, (rest, goals))
        | ([], (Requirement goal) :: t) when generateGoalCondition goal state -> 
            match makePlan state t with
            | Some newPlan -> nextAction state intent newPlan
            | None -> None
        | ([], (Requirement goal) :: t) when not <| generateGoalCondition goal state ->
            let (_,goals) = plan
            match makePlan state goals with
            | Some newPlan -> nextAction state intent newPlan
            | None -> None
        | ([], (MultiGoal goalFun) :: t) when goalTest (goalFun state) state -> 
            match makePlan state t with
            | Some newPlan -> nextAction state intent newPlan
            | None -> None
        | ([], (MultiGoal goals) :: t) -> 
            match makePlan state (snd plan) with
            | Some newPlan -> nextAction state intent newPlan
            | None -> None
        | ([], (Plan p) :: t) ->
            match makePlan state t with
            | Some newPlan -> nextAction state intent newPlan
            | None -> None
        | _ -> None

    type AgentPlanner() =
        class
            interface Planner<State, AgentAction, Intention, Plan> with 
                member self.FormulatePlan (state, intent) = 
                    formulatePlan state intent
                member self.RepairPlan (state, intent, solution) = 
                    repairPlan state intent solution
                member self.SolutionFinished (state, intent, solution) = 
                    solutionFinished state intent solution
                member self.NextAction (state, intent, solution) = 
                    nextAction state intent solution
        end
 
