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
       
    let formulatePlan state goal = 
        let solution = solve aStar <| agentProblem state goal
        match solution with
        | Some sol -> Some <| List.map perform sol.Path
        | None -> None

    let planWorking state goal plan =
        true

    let repairPlan state goal plan = None

    let solutionFinished state goal = 
        goal state
    
    let nextAction state goal = function
        | head :: tail -> (head, tail)
        | [] -> failwith "No next action for empty plan"

    type AgentPlanner()  =  // : FsPlanning.Agent.Planning.Planner<State, ActionSpecification, (State -> bool), Action list> = 
        class
            interface Planner<State, AgentAction, Intention, Solution> with 
                member self.FormulatePlan (state, intent) = 
                    let (_,_,goals) = intent
                    match goals with
                    | (Plan p)::_ -> Some (0, (p state))
                    | (Requirement r)::_ -> 
                        let plan = (formulatePlan state r)
                        match plan with
                        | Some p -> Some (0, p)
                        | None -> None
                    | [] -> Some (0, [])
                member self.PlanWorking (state, intent, solution) = planWorking state intent solution
                member self.RepairPlan (state, intent, solution) = repairPlan state intent solution
                member self.SolutionFinished (state, intent, solution) = 
                    let (_,_,goals) = intent
                    let (idx,plan) = solution
                    match plan with
                    | [] -> (List.length goals - 1) = idx
                    | _ -> false
                    //false //solutionFinished state intent
                member self.NextAction (state, intent, solution) = 
                    let (idx,plan) = solution
                    let (act, newPlan) = nextAction state intent plan
                    (act,(idx,newPlan))
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
 
