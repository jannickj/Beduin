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
                    | (_, Communication, _)
                    | (_, Inherent, _) -> Harmonic
                    | (_, Activity, _) -> match intB with
                                            | (_, Activity, _) -> Conflictive
                                            | _ -> Harmonic

                override this.OptimizeState(curState) =
                    if curState.UpdateMap then
                        logImportant <| sprintf "Updating Heuristic Map on %A nodes" curState.World.Count
                        let r = {curState with HeuristicMap = allPairsDistances curState.World}
                        logImportant "Done updating Heuristic Map"                   
                        r
                    else
                        curState
                override this.ImplementOptimizedState(curState,optState)=
                    if curState.HeuristicMap.Count < optState.HeuristicMap.Count then
                        { curState with HeuristicMap = optState.HeuristicMap }
                    else
                        curState
    end
