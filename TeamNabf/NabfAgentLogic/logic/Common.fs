namespace NabfAgentLogic
module Common =

    open FsPlanning.Agent.Planning
    open AgentTypes

    let PerceptsToShare (s:State) = 
        s.NewKnowledge
            
    let SharePercept (s:State) : Option<Intention> =
         Some ("ShareKnowledge", Communication, [Plan (fun s -> [(Communicate ( ShareKnowledge ( PerceptsToShare s)))] )])

    //An agent always wants to have exactly one goal
    let onlyOneJob s = Some("have exactly 1 job.",Inherent,[Requirement(fun state -> state.Jobs.Length = 1)])

    let exploreMap (s:State) = 
        if s.ExploredCount < s.TotalNodeCount
        then
            let count = s.MyExploredCount
            Some("explore one more node.",Activity,[Requirement(fun state -> state.MyExploredCount > count)])
        else
            None
