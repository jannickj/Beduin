namespace NabfAgentLogic
    open FsPlanning.Agent
    open FsPlanning.Agent.Planning
    open AgentTypes
    open Graphing.Graph
    open NabfAgentLogic.Logging
    open HandlePercepts
    open NabfAgentLogic.Planning
    open NabfAgentLogic.Search.HeuristicDijkstra

    type BDIAgentImpl(State,DesireTree,Planner) =
        class
           
            inherit BDIAgent<Percept,State,AgentAction,Intention,Plan>(State,DesireTree,Planner)
                override this.AnalyzePercept(percepts, state) = 
                    let newstate = updateState state percepts
                    newstate
            
                override this.FilterIntention(intA, intB) = 
                    match intA with
                    | (id, Communication, _) -> match (id,intB) with
                                                | (ida,(idb,Communication,_)) -> Conflictive
                                                | _ -> Harmonic
                    | (_, Inherent, _) -> Harmonic
                    | (_, Activity, _) -> match intB with
                                            | (_, Activity, _) -> Conflictive
                                            | _ -> Harmonic
                
                override this.IsIntentionEqual ((S1,_,_),(S2,_,_)) = S1 = S2

                override this.OptimizeState(curState) = curState
                override this.ImplementOptimizedState(curState,optState)= curState
    end
