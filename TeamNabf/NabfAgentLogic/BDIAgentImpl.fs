namespace NabfAgentLogic
    open FsPlanning.Agent
    open FsPlanning.Agent.Planning
    open AgentTypes

    type BDIAgentImpl(State,DesireTree,Planner) =
        inherit BDIAgent<Percept,State,AgentAction,Intention,Solution>(State,DesireTree,Planner)
            
            override this.AnalyzePercept(percept, state) = 
                match percept with
                | KnowledgeSent pl -> 
                    let updatedNK = List.filter (fun p -> List.exists ((=) p) pl) state.NewKnowledge
                    { state with NewKnowledge = updatedNK }
                | _ -> state
            override this.FilterIntention(intA, intB) = Conflictive
