namespace NabfAgentLogic
module AgentPlanning =
    open FsPlanning.Searching
    open ActionSpecifications
    open NabfAgentLogic.AgentTypes
    open Graphing.Graph
    open FsPlanning.Agent.Planning

    let agentProblem (state : State) goalTest = 
        { InitialState = state
        ; GoalTest     = goalTest
        ; Actions      = fun state -> List.filter (isApplicable state) (roleActions state)
        ; Result       = fun state action -> action.Effect state
        ; StepCost     = fun state action -> action.Cost state
        }

//    abstract member FormulatePlan : ('TState * 'TGoal) -> 'TSolution option
//    abstract member PlanWorking : ('TState * 'TGoal * 'TSolution) -> bool
//    abstract member RepairPlan : ('TState * 'TGoal * 'TSolution) -> 'TSolution option
//    abstract member SolutionFinished : ('TState * 'TGoal * 'TSolution) -> bool
//    abstract member NextAction : ('TState * 'TGoal * 'TSolution) -> 'TAction * 'TSolution

    let perform actionspec = Perform actionspec.ActionType
       
    let formulatePlan state intent = 
        let (_,_,goals) = intent
        match goals with
        | (Plan p)::_ -> Some (0, (p state))
        | (Requirement r)::_ -> 
            let plan = solve aStar <| agentProblem state r
            match plan with
            | Some sol -> Some (0,List.map perform sol.Path)
            | None -> None
        | [] -> Some (0, [])
        
    let planWorking state intent solution =
        let (_,_,goals) = intent
        let (idx,plan) = solution
        true

    let repairPlan state goal plan = None

    let solutionFinished state intent solution = 
        let (_,_,goals) = intent
        let (idx,plan) = solution
        match plan with
        | [] -> 
            let islastgoal = (List.length goals - 1) = idx
            if islastgoal then
                let finalgoal = goals.[idx]
                match finalgoal with
                | Requirement r -> r state
                | _ -> true
            else
                false    
        | _ -> false
    
    let nextAction state intent solution =
        let (idx,plan) = solution
        match plan with
        | act::rest -> (act,(idx,rest))
        | [] -> failwith "No next action for empty plan"

    type AgentPlanner()  =  // : FsPlanning.Agent.Planning.Planner<State, ActionSpecification, (State -> bool), Action list> = 
        class
            interface Planner<State, AgentAction, Intention, Solution> with 
                member self.FormulatePlan (state, intent) = formulatePlan state intent                    
                member self.PlanWorking (state, intent, solution) = planWorking state intent solution
                member self.RepairPlan (state, intent, solution) = repairPlan state intent solution
                member self.SolutionFinished (state, intent, solution) = solutionFinished state intent solution
                member self.NextAction (state, intent, solution) = nextAction state intent solution
        end
                    
//      type ProgressionPlanner() =
//        class
//            interface Planner<State, AgentAction, Intention, Solution> with
//                member this.FormulatePlan(state, goal) = None
//                member this.PlanWorking(state, goal, solution) = true
//                member this.RepairPlan (state, goal, solution) = None
//                member this.SolutionFinished (state, goal, solution) = false
//                member this.NextAction (state, goal, solution) = (Perform Skip,"Some Solution")
//
//        end
 
