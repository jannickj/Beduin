namespace NabfAgentLogic
    open FsPlanning.Agent.Planning
    open AgentTypes

    type ProgressionPlanner() =
        class
            interface Planner<State, AgentAction, Intention, Solution> with
                member this.FormulatePlan(state, goal) = None
                member this.PlanWorking(state, goal, solution) = true
                member this.RepairPlan (state, goal, solution) = None
                member this.SolutionFinished (state, goal, solution) = false
                member this.NextAction : ('TState * 'TGoal * 'TSolution) -> 'TAction * 'TSolution

        end
 
