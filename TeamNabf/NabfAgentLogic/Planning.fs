namespace NabfAgentLogic
module Planning =
    open ActionSpecifications
    open FsPlanning.Searching
    open NabfAgentLogic.AgentTypes
    open Graphing.Graph
    open FsPlanning.Agent.Planning
    open Logging

    type Plan = (ActionSpecification list) * (Goal list)

    let flip f x y = f y x

    let agentProblem (state : State) goalTest = 
        { InitialState = state
        ; GoalTest     = goalTest
        ; Actions      = fun state -> List.filter (isApplicable state) (roleActions state)
        ; Result       = fun state action -> action.Effect state
        ; StepCost     = fun state action -> action.Cost state
        }

    //let perform actionspec = Perform actionspec.ActionType

    let makePlan state goals =
        match goals with
        | (Plan plan) :: _ -> Some (List.map actionSpecification <| plan state, goals)
        | (Requirement r) :: _ -> 
            let plan = solve aStar <| agentProblem state r
            match plan with
            | Some sol -> 
                let actions = sol.Path
                logImportant (sprintf "Found plan: %A" actions)                
                Some (actions, goals)
            | None -> None
        | [] -> Some ([], goals)
       
    let formulatePlan (state : State) intent = 
        let (name,_,goals) = intent
        logImportant ("Planning to " + name)
        makePlan state goals

    let rec repairPlanHelper state plan = 

        let restPlan state action actionList =
            let restOfPlan = repairPlanHelper state actionList
            match restOfPlan with
                | Some plan -> Some <| action :: plan
                | None -> None

        match plan with
        | action :: tail when isApplicable state action ->
            restPlan state action tail
        | action :: tail ->
            let gluePlan = solve aStar <| agentProblem state (flip isApplicable action)
            match gluePlan with
            | Some solution -> restPlan state action (solution.Path @ tail)
            | None -> None
        | _ -> Some plan

    let repairPlan state intent (plan : Plan) = 
        let (path, goals) = plan
        let newPlan = repairPlanHelper state path

        match newPlan with
        | Some p -> Some (p, goals)
        | None -> None

    let solutionFinished state intent solution = 
        match solution with
        | (_, [Requirement req]) -> req state
        | (_, []) -> true
        | _ -> false
    
    let rec nextAction state (intent : Intention) (plan : Plan) =
        match plan with
        | (action :: rest, goals) -> Some (action.ActionType, (rest, goals))
        | ([], (Requirement req) :: t) when req state -> 
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
 
