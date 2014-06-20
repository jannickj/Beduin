namespace NabfAgentLogic
    open FsPlanning.Agent
    open FsPlanning.Agent.Planning
    open AgentTypes
    open Graphing.Graph
    open NabfAgentLogic.Logging
    open NabfAgentLogic.Perception
    open NabfAgentLogic.Planning
    open NabfAgentLogic.Search.HeuristicDijkstra

    type BDIAgentImpl(State,DesireTree,Planner) =
        class
           
            inherit BDIAgent<Percept,State,AgentAction,Intention,Plan>(State,DesireTree,Planner)
                override this.AnalyzePercept(percepts, state) = 
                    let newstate = AnalyzePercepts.updateState state percepts
                    newstate
            
                override this.FilterIntention(intA, intB) = 
                    match (intA.Label,intA.Type) with
                    | (id, Communication) -> match (id,(intB.Label,intB.Type)) with
                                                | (ida,(idb,Communication)) -> Conflictive
                                                | _ -> Harmonic
                    | (_, Inherent) -> Harmonic
                    | (_, Activity) -> match (intB.Label,intB.Type) with
                                            | (_, Activity) -> Conflictive
                                            | _ -> Harmonic
                
                override this.IsIntentionEqual (intA,intB) = intA.Label = intB.Label

                override this.OptimizeState(curState) = curState
                override this.ImplementOptimizedState(curState,optState)= curState
    end
