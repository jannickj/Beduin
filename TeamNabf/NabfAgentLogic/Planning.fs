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

    let formulatePlan state goal = 
        solve aStar <| agentProblem state goal

    let planWorking state goal plan =
        true

    let repairPlan state goal plan = Some plan

    let solutionFinished state goal = 
        goal state
    
    let nextAction state goal = function
        | head :: tail -> (head, tail)
        | [] -> failwith "No next action for empty plan"

    type AgentPlanner(state : State, goal : (State -> bool))  =  // : FsPlanning.Agent.Planning.Planner<State, ActionSpecification, (State -> bool), Action list> = 
        interface Planner<State, ActionSpecification, (State -> bool), Action list> with 
            member self.FormulatePlan ((state, goal)) = formulatePlan state goal
            member self.PlanWorking ((state, goal, plan)) = planWorking
            member self.RepairPlan ((state, goal, plan)) = repairPlan state goal plan
            member self.SolutionFinished ((state, goal, solution)) = solutionFinished state goal
            member self.NextAction ((state, goal, solution)) = solutionFinished state goal
            
       