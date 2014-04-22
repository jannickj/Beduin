namespace NabfAgentLogic
    open FsPlanning.Agent
    open FsPlanning.Agent.Planning
    open AgentTypes

    type BDIAgentImpl(State,DesireTree,Planner) =
        class
            inherit BDIAgent<Percept,State,AgentAction,Intention,Solution>(State,DesireTree,Planner)
            
                override this.AnalyzePercept(percepts, state) = 
                    match List.head percepts with
                    | KnowledgeSent pl -> 
                        
                        let updatedNK = List.filter (fun p -> List.exists ((=) p) pl) state.NewKnowledge
                        { state with NewKnowledge = updatedNK }
                    | _ -> state
                override this.FilterIntention(intA, intB) = Conflictive
        
        end
