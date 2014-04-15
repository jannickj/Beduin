namespace NabfAgentLogic
    open FsPlanning.Agent
    open FsPlanning.Agent.Planning
    open AgentTypes

    type BDIAgentImpl(State,DesireTree,Planner) =
        inherit BDIAgent<Percept,State,AgentAction,Intention,Solution>(State,DesireTree,Planner)
            
            override this.AnalyzePercept(percept, state) = state
            override this.FilterIntention(intA, intB) = Conflictive
