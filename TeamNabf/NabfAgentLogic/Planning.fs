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

    type Plan = (ActionSpecification list) * (Objective list)
    let test = 1
    let flip f x y = f y x

    let wrappedGoalTest goalTest state = 
        try goalTest state with
        | ex -> logError <| sprintf "goal test: %A \nfailed with:\n %A" goalTest ex
                false

    let distance (goals : Goal list) state cost =
        let heuristics = 
            List.choose id 
                ( List.map (fun (_,heuOpt) ->
                            match heuOpt with
                            | Some heuFunc -> Some <| heuFunc state
                            | None ->  None
                           ) goals
                )
        match heuristics with 
           | [] -> 0
           | [single] -> single
           | multi -> List.min multi 
      

    let goalCount (goalFun : State -> Goal list) state =
        List.length <| List.filter (fun (func,_) -> not <| func state) (goalFun state)

    let h goalFun state cost = 
        let goals = goalFun state
        (goalCount goalFun state, cost + distance goals state cost)
    
    let goalTest goalFun state = 
        List.forall (fun (func,_) -> func state) <| goalFun state
            
    let timedGoalTest breakTime (goalFun : State -> Goal list) state = 
        let someGoalSatisfied = List.exists (fun (func,_) -> func state) <| goalFun state
        if System.DateTime.Now >= breakTime then
            true
        else
            goalTest goalFun state
        
    let goalFunc goal = 
        match goal with
        | MultiGoal func  -> func 
        | Requirement req -> fun _ -> [req]
        | Plan _ -> failwith "Trying to search on predefined plan"

    let agentProblem (state : State) goal = 
        let breakTime = System.DateTime.Now + System.TimeSpan.FromMilliseconds Constants.MAX_PLANNING_TIME_MS
        { InitialState = state
        ; GoalTest     = wrappedGoalTest <| timedGoalTest breakTime (goalFunc goal)
        ; Actions      = fun state -> List.filter (isApplicable state) (roleActions state)
        ; Result       = fun state action -> action.Effect state
        ; StepCost     = fun state action -> action.Cost state
        ; Heuristic    = h (goalFunc goal)
        }

    //let perform actionspec = Perform actionspec.ActionType

    let rec prunePlanHelper path goal =
        let gC searchnode = goalCount (goalFunc goal) searchnode.State
        match path with
        | searchNode1 :: searchNode2 :: tail when gC searchNode1 < gC searchNode2 ->
            List.rev <| searchNode2 :: tail
        | sn1 :: sn2 :: tail -> prunePlanHelper (sn2 :: tail) goal
        | [sn] -> [sn]
        | [] -> []

    let prunePlan plan goals = 
        match plan with
        | Some {Cost = _; Path = path} ->Some <| prunePlanHelper (List.rev path) goals
        | None -> None

    let makePlan initstate goals =
        let state = { initstate with LastAction = Skip }
        match goals with
        | (Plan plan) :: _ -> Some (List.map actionSpecification <| plan state, goals)
        | goal :: _ -> 
            let plan = solveSearchNodePath aStar <| agentProblem state goal
            let prunedPlan = prunePlan plan goal
            match prunedPlan with 
            | Some path -> 
                let actions = List.map (fun node -> node.Action.Value) path
                logImportant (sprintf "Found plan: %A" <| List.map (fun action -> action.ActionType) actions)
                Some (actions, goals)
            | None -> None
        | [] -> Some ([], goals)

       
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
            let gluePlan = makePlan state ([Requirement <| ((flip isApplicable action), None)])

            match gluePlan with
            // If we find a non-empty glue plan [a; b; c], prepend it to the plan and continue recursing
            | Some (newAction :: newTail, _) -> 
                logInfo <| sprintf "Found glue plan %A" (List.map (fun action -> action.ActionType) (newAction :: newTail))
                restPlan (newAction.Effect state) newAction (newTail @ action :: tail)
            | Some (_, _) -> restPlan (action.Effect state) action tail
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
        | (_, [Requirement (req,_)]) ->  wrappedGoalTest req state
        | (_, [MultiGoal goalFun]) -> wrappedGoalTest (goalTest goalFun) state
        | (_, []) -> true
        | _ -> false
    
    let rec nextAction state (intent : Intention) (plan : Plan) =
        match plan with
        | (action :: rest, goals) -> Some (action.ActionType, (rest, goals))
        | ([], (Requirement (req,_)) :: t) when req state -> 
            match makePlan state t with
            | Some newPlan -> nextAction state intent newPlan
            | None -> None
        | ([], (Requirement (req,_)) :: t) when not <| req state ->
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
 
