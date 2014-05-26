namespace NabfAgentLogic
    open FsPlanning.Agent
    open FsPlanning.Agent.Planning
    open AgentTypes
    open Graphing.Graph
    open NabfAgentLogic.Logging
    open HandlePercepts
    open NabfAgentLogic.Planning

    type BDIAgentImpl(State,DesireTree,Planner) =
        class
           
            inherit BDIAgent<Percept,State,AgentAction,Intention,Plan>(State,DesireTree,Planner)
                override this.AnalyzePercept(percepts, state) = 
                    let newstate = updateState state percepts
                    newstate
            
                override this.FilterIntention(intA, intB) = 
                    match intA with
                    | (_, Communication, _)
                    | (_, Inherent, _) -> Harmonic
                    | (_, Activity, _) -> match intB with
                                            | (_, Activity, _) -> Conflictive
                                            | _ -> Harmonic

            
        
    end
