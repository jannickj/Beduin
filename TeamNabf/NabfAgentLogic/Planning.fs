namespace NabfAgentLogic
module Planning =
    open ActionSpecifications
    open FsPlanning.Search.Problem
    open FsPlanning.Search.Astar
    open NabfAgentLogic.AgentTypes
    open Graphing.Graph
    open FsPlanning.Agent.Planning
    open Logging

    type Plan = (ActionSpecification list) * (Goal list)
    let test = 1
    let flip f x y = f y x
    let wrappedGoalTest goalTest state = 
        try goalTest state with
        | ex -> logError <| sprintf "goal test: %A \nfailed with:\n %A" goalTest ex
                false

    let testfun state = 
        let actions = roleActions state
        let unsat = unSatisfiedPreconditions state (List.head actions)
        logError <| sprintf "%A" unsat

    let goalCount goalFun state cost =
        let res = List.length <| List.filter (fun func -> not <| func state) (goalFun state)
        let len = List.length <| goalFun state
        logImportant <| sprintf "(%A / %A) goals satisfied" (len - res) len
        res

    
    let goalTest goalFun state = 
        List.forall (fun func -> func state) <| goalFun state

    let agentProblem (state : State) goal = 
        
        let goalFunc = 
            match goal with
            | MultiGoal func  -> func 
            | Requirement req -> fun _ -> [req]
            | Plan _ -> failwith "Trying to search on predefined plan"

        { InitialState = state
        ; GoalTest     = wrappedGoalTest <| goalTest goalFunc
        ; Actions      = fun state -> testfun state; List.filter (isApplicable state) (roleActions state)
        ; Result       = fun state action -> action.Effect state
        ; StepCost     = fun state action -> action.Cost state
        ; Heuristic    = goalCount goalFunc
        }

    //let perform actionspec = Perform actionspec.ActionType

    let makePlan initstate goals =
        let state = { initstate with LastAction = Skip }
            

        if state.Self.Node <> "" then
            match goals with
            | (Plan plan) :: _ -> Some (List.map actionSpecification <| plan state, goals)
            | goal :: _ -> 
                let plan = solve aStar <| agentProblem state goal
                match plan with
                | Some sol -> 
                    let actions = sol.Path
                    logImportant (sprintf "Found plan: %A" <| List.map (fun action -> action.ActionType) actions)                
                    Some (actions, goals)
                | _ -> None
            | [] -> Some ([], goals)
        else None
       
    let formulatePlan (state : State) intent = 
        let (name, inttype, goals) = intent
        match inttype with
        | Communication -> 
            logInfo ("Sending message " + name)
        | Activity ->
            logImportant ("Planning to " + name)
        | _ -> ()
        makePlan state goals

    let rec repairPlanHelper state plan = 

        let restPlan state action actionList =
            let restOfPlan = repairPlanHelper state actionList
            match restOfPlan with
            | Some plan -> Some <| action :: plan
            | None -> None

        match plan with
        | action :: tail when isApplicable state action ->
            restPlan (action.Effect state) action tail
        | action :: tail ->
            logImportant <| sprintf "Inconsistency found! state does not satisfy %A" action.ActionType
            logInfo <| sprintf "the following errors were found: %A" (unSatisfiedPreconditions state action)
            let gluePlan = solve aStar <| agentProblem state (Requirement <| flip isApplicable action)
            match gluePlan with
            | Some {Cost = _; Path = []} -> restPlan (action.Effect state) action tail
            // If we find a non-empty glue plan [a; b; c], prepend it to the plan and continue recursing
            | Some {Cost = _; Path = newAction :: newTail} -> 
                logInfo <| sprintf "Found glue plan %A" (List.map (fun action -> action.ActionType) (newAction :: newTail))
                restPlan (newAction.Effect state) newAction (newTail @ action :: tail)
            | None ->
                logImportant <| sprintf "Failed to find glue plan" 
                None
        | _ -> Some plan

    let repairPlan state intent (plan : Plan) = 
        let (path, goals) = plan
        logInfo <| sprintf "Repairing plan %A" (List.map (fun action -> action.ActionType) path)
        let newPlan = repairPlanHelper state path

        match newPlan with
        | Some p -> 
            logInfo <| sprintf "repaired plan: %A" (List.map (fun action -> action.ActionType) p)
            Some (p, goals)
        | None -> None

    let solutionFinished state intent solution = 
        match solution with
        | (_, [Requirement req]) ->  wrappedGoalTest req state
        | (_, [MultiGoal goalFun]) -> wrappedGoalTest (goalTest goalFun) state
        | (_, []) -> true
        | _ -> false
    
    let rec nextAction state (intent : Intention) (plan : Plan) =
        match plan with
        | (action :: rest, goals) -> Some (action.ActionType, (rest, goals))
        | ([], (Requirement req) :: t) when req state -> 
            match makePlan state t with
            | Some newPlan -> nextAction state intent newPlan
            | None -> None
        | ([], (Requirement req) :: t) when not <| req state ->
            let (_,goals) = plan
            match makePlan state goals with
            | Some newPlan -> nextAction state intent newPlan
            | None -> None
        | ([], (MultiGoal goals) :: t) when goalTest goals state -> 
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
 
