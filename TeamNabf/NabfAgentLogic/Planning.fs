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
        | Some {Cost = _; Path = path} -> 
            match path with
            | _ -> Some <| prunePlanHelper (List.rev path) goals
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

            let isSomePathValid path = 
                not <| List.forall (fun node -> satisfiedGoalCount (goalList goalObjective node.State) node.State = 0) path

            match plan with
            | Some {Cost = _; Path = []} -> Some ([], objectives)
            | Some {Cost = _; Path = path} when isSomePathValid path -> 
                let actions = List.map (fun node -> node.Action.Value) path
                logImportant (sprintf "Found plan: %A" <| List.map (fun action -> action.ActionType) actions)
                Some (actions, objectives)
            | Some {Cost = _; Path = path} ->
                let actions = List.map (fun node -> node.Action.Value) path
                logImportant <| sprintf "Discarded plan %A with objective %A" (List.map (fun action -> action.ActionType) actions) goalObjective
                None
            | _ ->
                logImportant <| sprintf "No plan found for intention %A" goalObjective
                None
        | [] -> Some ([], objectives)

    let repairPlan (state : State) intent (originalPlan : Plan) = 
        logImportant <| sprintf "Node: %A (%A)" state.Self.Node state.LastPosition
        logImportant <| sprintf "last action: %A (%A)" state.LastAction state.LastActionResult
        let plan' = 
            match originalPlan with
            | (action :: tail, objectives) when state.LastActionResult <> FailedRandom || action = skipAction -> 
                (tail, objectives)
            | (action :: _, _) ->
                originalPlan
            | ([], _) -> originalPlan

        let plan = 
            match plan' with
            | (action :: rest, _) when isApplicable state action ->
                plan'
            | (action :: rest, objectives) when isApplicable (rechargeAction.Effect state) action ->
                (rechargeAction :: action :: rest, objectives)
            | _ -> plan'

        logImportant <| sprintf "repairing plan %A" (List.map (fun action -> action.ActionType) (fst plan))

        let rechargedState (state : State) = {state with Self = {state.Self with Energy = state.Self.MaxEnergy}}

        let rec workingPlan state plan =
            match plan with
            | (action :: tail, objectives) when isApplicable (rechargedState state) action ->
                Option.map (List.append [action]) <| workingPlan (action.Effect state) (tail, objectives)
            | (action :: tail, objectives) -> 
                Some []
            | ([], objective :: tail) when goalTest (goalList objective state) state ->
                None
            | _ -> None

        let planToMinHeuristic state objective plan =
            
            let heuristics (state, heu, cost) action = 
                let state' = action.Effect state 
                let cost' = action.Cost state + cost
                let heu' = h objective state' cost'
                (state', heu', cost')

            let heuList = List.scan heuristics (state, (0, 0), 0) plan

            let minHeu = List.minBy (fun (_, heu, _) -> heu) heuList
            let minHeuIdx = List.findIndex ((=) minHeu) heuList

            let rec toNth ls nth count =
                if count = nth then
                    ls
                else 
                    toNth (List.tail ls) nth (count + 1)

            toNth plan minHeuIdx 0

        match workingPlan state plan with           
        | Some p -> 
            let objective = List.head (snd plan)
            let path = fst plan
            let prunedPlan = planToMinHeuristic state (goalList objective state) path
            let fromState = List.fold (fun state action -> action.Effect state) state prunedPlan

            match makePlan fromState (snd plan) with
            | Some (path, objectives) ->
                logImportant <| sprintf "repaired plan: %A" (List.map (fun action -> action.ActionType) path)
                Some (prunedPlan @ path, objectives)
            | None -> None

        | None -> 
            logImportant <| sprintf "kept plan: %A" (List.map (fun action -> action.ActionType) (fst plan))
            Some plan

    let formulatePlan (state : State) intent = 
        let (name, inttype, goals) = intent
        match inttype with
        | Communication -> 
            logInfo ("Sending message " + name)
        | Activity ->
            logImportant ("Planning to " + name)
        | _ -> ()

//        makePlan state goals
        match makePlan state goals with
        | Some (path, Plan p :: objectives) -> Some (path, Plan p :: objectives)
        | Some (path, objectives) -> Some (skipAction :: path, objectives)
        | None -> None

    let solutionFinished state intent solution = 
        match solution with
        | (_, [Plan plan]) -> 
            match plan state with
            | Some [] 
            | None -> true
            | _ -> false
        | (_, [objective]) ->  (wrappedGoalTest <| goalTest (goalList objective state)) state
        | (_, []) -> true
        | _ -> false
    
    let rec nextAction state (intent : Intention) (plan : Plan) =
        match plan with
        | (action :: rest, goals) -> Some (action.ActionType, (action :: rest, goals))
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
                    match solution with
                    | (_, Plan _ :: _) -> Some solution
                    | _ -> repairPlan state intent solution
                member self.SolutionFinished (state, intent, solution) = 
                    solutionFinished state intent solution
                member self.NextAction (state, intent, solution) = 
                    let nextAction' = nextAction state intent solution
//                    logImportant <| sprintf "next action: %A" nextAction'
                    nextAction'
                member self.UpdateStateBeforePlanning (state, intent) = state
                
                member self.UpdateStateOnSolutionFinished (state, intent, solution) = state
        end
 
