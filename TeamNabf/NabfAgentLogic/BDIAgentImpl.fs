namespace NabfAgentLogic
    open FsPlanning.Agent
    open FsPlanning.Agent.Planning
    open AgentTypes
    open Graphing.Graph
    open NabfAgentLogic.Logging
    open HandlePercepts

    type BDIAgentImpl(State,DesireTree,Planner) =
        class
           
            inherit BDIAgent<Percept,State,AgentAction,Intention,Solution>(State,DesireTree,Planner)
            override this.AnalyzePercept(percepts, state) = updateState state percepts
            override this.FilterIntention(intA, intB) = Conflictive
        end
