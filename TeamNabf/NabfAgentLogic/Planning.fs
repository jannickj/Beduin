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
    open NabfAgentLogic.Search.HeuristicDijkstra
    open GeneralLib
    open AgentTypes

    type Plan = (ActionSpecification list) * (Objective list)

    // Transforms objectives into a normalized form of State -> (Goal list)
    let goalFunc objective = 
        match objective with
        | MultiGoal func  -> func 
        | Requirement req -> fun _ -> [req]
        | Plan _          -> failwith "Trying to search on predefined plan"

    let goalList objective state = goalFunc objective <| state

    let wrappedGoalTest goalTest state = 
        try goalTest state with
        | ex -> logStateError state Planning <| sprintf "goal test: %A \nfailed with:\n %A\nstack trace:\n%A" goalTest ex.Message ex.StackTrace
                false

    let distance (goals : Goal list) state cost =
        match goals with
        | [] -> 0
        | gl -> List.min <| List.map (fun goal -> goalHeuristics goal <| state) gl 
      
    let satisfiedGoalCount goals state =
        List.length <| List.filter ((flip generateGoalCondition) state) goals

    let unSatisfiedGoalCount goals state =
        List.length <| List.filter (not << (flip generateGoalCondition) state) goals

    let h goals state cost = 
        (unSatisfiedGoalCount goals state, cost + distance goals state cost)
    
    let goalTest goals state = 
        List.forall (fun goal -> (generateGoalCondition goal) state) goals

    let applicableActions goals state =
        let actions = Set.ofList <| List.collect (availableActions state) goals
        let appActs = Set.filter (isApplicable state) actions
        Set.toList appActs

    let agentProblem (state : State) goals = 
        let headGoal = List.head goals

        { InitialState = state
        ; GoalTest     = wrappedGoalTest <| goalTest goals
        ; Actions      = applicableActions goals
        ; Result       = fun state action -> action.Effect state
        ; StepCost     = fun state action -> action.Cost state
        ; Heuristic    = h goals
        }

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
            | Some {Cost = _; Path = []} -> 
                logStateImportant state Planning <| sprintf "Found empty plan for goals %A" goals
                Some ([], objectives)
            | Some {Cost = _; Path = path} when isSomePathValid path -> 
                let actions = List.map (fun node -> node.Action.Value) path
                logStateImportant state Planning (sprintf "Found plan: %A" <| List.map (fun action -> action.ActionType) actions)
                Some (actions, objectives)
            | Some {Cost = _; Path = path} ->
                let actions = List.map (fun node -> node.Action.Value) path
                logStateImportant state Planning <| sprintf "Discarded plan %A" (List.map (fun action -> action.ActionType) actions) //with objective %A" goalObjective
                None
            | None ->
                logStateImportant state Planning <| sprintf "No plan found for intention %A" goalObjective
                None
        | [] -> Some ([], [])

    let maybePrependRechargeAction state plan = 
        match plan with
        | action :: _ when isApplicable state action ->
            Some plan
        | action :: rest when isApplicable (rechargeAction.Effect state) action ->
            Some <| rechargeAction :: plan
        | [] -> Some plan
        | _ -> 
            None
              
    let maybeSkipFirstAction state plan = 
        match plan with 
        | action :: rest ->
            match action.ActionType with
            | Communicate _ -> rest
            | Perform _ when state.LastActionResult = Successful || action = skipAction ->
                rest
            | _ -> plan
        | [] -> plan

    let preprocessPlan state plan =
        maybeSkipFirstAction state plan 
        |> maybePrependRechargeAction state


    let repairNormalPlan (state : State) (originalPlan : Plan) = 
//        logImportant <| sprintf "Node: %A (%A)" state.Self.Node state.LastPosition
//        logImportant <| sprintf "last action: %A (%A)" state.LastAction state.LastActionResult
        match state.LastActionResult with
        | Successful | FailedRandom | FailedInRange | FailedParried -> ()
        | err -> logStateError state Planning <| sprintf "Last action result was %A, trying to repair plan anyway" err

        let objectives = snd originalPlan

        let tmpPlan = maybeSkipFirstAction state (fst originalPlan)

        let plan = 
            match maybePrependRechargeAction state tmpPlan with
            | Some plan -> (plan, objectives)
            | None -> (tmpPlan, objectives)

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
            
            let heuristics (state, _, cost) action = 
                let state' = action.Effect state 
                let cost' = action.Cost state + cost
                let heu' = h objective state' cost'
                (state', heu', cost')

            let initialState = (state, (h objective state 0), 0)

            let heuList = List.scan heuristics initialState plan

            let minHeu = List.minBy (fun (_, heu, _) -> heu) heuList
            let minHeuIdx = List.findIndex ((=) minHeu) heuList

            let rec toNth ls nth count =
                match ls with
                | [] -> []
                | _ when nth = count -> []
                | head :: tail -> head :: toNth tail nth (count + 1)

            toNth plan minHeuIdx 0

        match workingPlan state plan with           
        | Some p -> 
            let objective = List.head (snd plan)
            let prunedPlan = planToMinHeuristic state (goalList objective state) p
            let fromState = List.fold (fun state action -> action.Effect state) state prunedPlan

            match makePlan fromState (snd plan) with
            | Some (path, objectives) ->
                logStateImportant state Planning <| sprintf "repaired plan: %A" (List.map (fun action -> action.ActionType) path)
                Some (prunedPlan @ path, objectives)
            | None -> None

        | None -> 
            logStateImportant state Planning <| sprintf "kept plan: %A" (List.map (fun action -> action.ActionType) (fst plan))
            Some plan

    let repairPlan state solution =
        match solution with
        | (plan, Plan p :: rest) -> 
            match maybeSkipFirstAction state plan |> maybePrependRechargeAction state with
            | Some plan -> Some (plan, Plan p :: rest)
            | None -> None
        | _ -> repairNormalPlan state solution

    let formulatePlan (state : State) (intent:Intention) = 
        let (name, inttype, goals) = (intent.Label, intent.Type, intent.Objectives)
        match inttype with
        | Communication -> 
            logStateInfo state Planning ("Sending message " + name)
        | Activity ->
            logStateImportant state Planning ("Planning to " + name)
        | _ -> ()

//        makePlan state goals
        match makePlan state goals with
        | Some (path, Plan p :: objectives) -> Some (skipAction :: path, Plan p :: objectives)
        | Some (path, objectives) -> Some (skipAction :: path, objectives)
        | None -> None

    let solutionFinished state intent (solution : Plan) = 
        match solution with
        | ([p], [Plan _]) -> true
        | ([], [Plan _]) -> true
        | (_, [Plan _]) -> false
        | (_, [objective]) ->
            (wrappedGoalTest <| goalTest (goalList objective state)) state
        | (_, []) -> true
        | _ -> false
    
    let rec nextAction state (intent : Intention) (plan : Plan) =
        let gtest objective = wrappedGoalTest (goalTest (goalList objective state)) state

        match immediateAction state with
        | Some action -> Some (action, plan)
        | None -> 
            match plan with
            | (action :: rest, Plan _ :: _) & (_, objectives) ->
                Some (action.ActionType, (action :: rest, objectives))
            | (action :: rest, objective :: _) & (_, objectives) when not <| gtest objective  -> 
                Some (action.ActionType, (action :: rest, objectives))
            | (_, objectives) & (_, objective :: _)  ->
                let newObjectives = 
                    match objectives with
                    | (Plan p) :: tail -> 
                        tail
                    | objective :: tail when gtest objective ->
                        tail
                    | objectives ->
                        objectives

                match makePlan state newObjectives with
                | Some newPlan -> nextAction state intent newPlan
                | None -> None

            | (_, [] ) -> None
           
    let updateGoalHeuristic goal state =
        match goalVertex goal state with
        | Some vertex -> updateHeuristic vertex state
        | None -> state

    let updateStateBeforePlanning initstate (intent:Intention) = 
        let state = match intent.ChangeStateBefore with
                    | Some stateFunc -> stateFunc initstate
                    | None -> initstate
        match (intent.Type, intent.Objectives) with
        | ( Activity, objective :: _) ->
            match objective with
            | Requirement goal ->
                updateGoalHeuristic goal state
            | MultiGoal goalfunc ->
                match goalfunc state with
                | first::_ -> 
                    updateGoalHeuristic first state
                | [] -> state
            | _ -> state
        | _ -> state

    let updateStateOnSolutionFinished initstate (intention:Intention) solution = 
        match intention.ChangeStateAfter with
        | Some stateFunc -> stateFunc initstate
        | None -> initstate

    type AgentPlanner() =
        class
            interface Planner<State, AgentAction, Intention, Plan> with 
                member self.FormulatePlan (state, intent) = 
                    let plan = 
                        try formulatePlan state intent with
                        | exn -> logStateError state Planning <| sprintf "Error encountered in formulatePlan: %A at %A" exn.Message exn.StackTrace
                                 None
                    plan
                member self.RepairPlan (state, intent, solution) =
                    let plan = 
                        try repairPlan state solution with
                        | exn -> logStateError state Planning <| sprintf "Error encountered in repairPlan: %A at %A" exn.Message exn.StackTrace
                                 None
                    plan
                    
                member self.SolutionFinished (state, intent, solution) = 
                    let result = 
                        try solutionFinished state intent solution with
                        | exn -> logStateError state Planning <| sprintf "Error encountered in solutionFinished: %A at %A" exn.Message exn.StackTrace
                                 false
                    result
                    
                member self.NextAction (state, intent, solution) = 
                    let action = 
                        try nextAction state intent solution with
                        | exn -> logStateError state Planning <| sprintf "Error encountered in nextAction: %A at %A" exn.Message exn.StackTrace
                                 None
                    action

                member self.UpdateStateBeforePlanning (state, intent) = 
                    let newState = 
                        try updateStateBeforePlanning state intent with
                        | exn -> logStateError state Planning <| sprintf "Error encountered in updateStateBeforePlanning: %A at %A" exn.Message exn.TargetSite
                                 state
                    newState
                
                member self.UpdateStateOnSolutionFinished (state, intent, solution) = 
                    let newState = 
                        try updateStateOnSolutionFinished state intent solution with
                        | exn -> logStateError state Planning <| sprintf "Error encountered in updateStateOnSolutionFinished: %A at %A" exn.Message exn.TargetSite
                                 state
                    newState
                    
        end
 
