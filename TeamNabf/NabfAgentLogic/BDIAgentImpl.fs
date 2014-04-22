namespace NabfAgentLogic
    open FsPlanning.Agent
    open FsPlanning.Agent.Planning
    open AgentTypes
    open Graphing.Graph
    open NabfAgentLogic.Logging

    type BDIAgentImpl(State,DesireTree,Planner) =
        inherit BDIAgent<Percept,State,AgentAction,Intention,Solution>(State,DesireTree,Planner)


        (* handlePercept State -> Percept -> State *)
        let handlePercept state percept =
            match percept with
                | EnemySeen enemy when enemy.Name <> state.Self.Name
                    -> 
                        let updateAgentData agentNew agentOld = 
                            let oldUpdated = match agentNew.Role with
                                                | Some role -> { agentOld with Role = agentNew.Role }
                                                | None -> agentOld
                            
                            let oldUpdated = { oldUpdated with Node = agentNew.Node }

                            let oldUpdated = { oldUpdated with Team = agentNew.Team }

                            let oldUpdated = { oldUpdated with Status = agentNew.Status }

                            oldUpdated          
                            
                        let updateAgentList agent alist = 
                            let oldAgentData = List.filter (fun a -> a.Name = agent.Name) alist                            
                            let others = List.filter (fun a -> a.Name <> agent.Name) alist

                            match oldAgentData with 
                            | [] -> agent::others
                            | [oldAgent] -> (updateAgentData agent oldAgent)::others
                            | _ -> raise(System.Exception("Duplicate data in EnemyData or FriendlyData, in handle percept function"))                            

                        if enemy.Team = state.Self.Team then
                            { state with FriendlyData = updateAgentList enemy state.FriendlyData }
                        else
                            { state with EnemyData = updateAgentList enemy state.EnemyData }
                
                | VertexProbed (name, value) ->
                    { state with 
                            World = addVertexValue name value state.World
                    }
                        
                | EdgeSeen (cost, node1, node2) ->
                    let edgeAlreadyExists = fun (cost':Option<_>, otherVertexId) -> cost'.IsSome && otherVertexId = node2

                    let containNode = (Map.containsKey node1 state.World)
                    //let edges = state.World.[node1].Edges
                    //logInfo ("Contains Node: "+containNode.ToString())
                    if ( not (containNode && (Set.exists edgeAlreadyExists state.World.[node1].Edges))) then
                        { state with 
                            World = addEdge (cost, node1, node2) state.World 
                            NewEdges = (cost, node1, node2) :: state.NewEdges
                        }
                    else
                        state

                | Team team ->
                    { state with 
                        TeamZoneScore = team.ZoneScore
                        Money = team.Money
                        LastStepScore = team.LastStepScore
                        Score = team.Score
                    }
                | SimulationStep step  -> { state with SimulationStep = step }
                | ZoneScore score      -> { state with ThisZoneScore = score }
                | Self self ->
                    let newSelf = { self with 
                                     Name = state.Self.Name
                                     Team = state.Self.Team
                                     Role = state.Self.Role
                    }
                    { state with Self = newSelf }
                | LastAction action    -> { state with LastAction = action }
                | LastActionResult res -> { state with LastActionResult = res }
                | _ -> state

        let clearTempBeliefs state =
            { state with 
                NewEdges = []
                NewVertices = []
            }

        let updateTraversedEdgeCost (oldState : State) (newState : State) =
            match (oldState.Self.Node, newState.LastAction, newState.LastActionResult) with
            | (fromVertex, Goto toVertex, Successful) -> 
                let edge = (Some (oldState.Self.Energy.Value - newState.Self.Energy.Value), fromVertex, toVertex)
                { newState with 
                    World = addEdge edge newState.World
                    NewEdges = edge :: newState.NewEdges 
                }
            | _ -> newState

        let updateEdgeCosts (lastState:State) (state:State) =
            match (state.LastAction, state.LastActionResult) with
            | (Goto _, Successful) ->
                let state4 = updateTraversedEdgeCost lastState state
                state4
            | _ -> updateTraversedEdgeCost lastState state

        let updateLastPos (lastState:State) (state:State) =
            { state with LastPosition = lastState.Self.Node }

        let updateJobs knownJobs (state:State) =
            { state with Jobs = knownJobs }

        let updateProbeCount (lastState:State) (state:State) =
            
            let probeAction = match state.LastAction with
                                | Action.Probe param -> true
                                | _ -> false
            let resultSuccessful = state.LastActionResult = ActionResult.Successful
            let getProbedVertex = lastState.Self.Node
            let notAlreadyProbed = lastState.World.[getProbedVertex].Value = Option.None

            if probeAction && resultSuccessful && notAlreadyProbed then 
                { state with 
                    MyProbedCount = state.MyProbedCount + 1    
                    ProbedCount = state.ProbedCount + 1
                }
            else
                state

        let updateExploredCount (lastState:State) (state:State) =
            
            let gotoAction = match state.LastAction with
                                | Action.Goto param -> true
                                | _ -> false
            let resultSuccessful = state.LastActionResult = ActionResult.Successful
            let getGotoVertex = match state.LastAction with
                                | Action.Goto node -> node
                                | _ -> ""
            let notAlreadyExplored = Set.forall (fun (value, _) -> value = Option.None) lastState.World.[getGotoVertex].Edges

            if gotoAction && resultSuccessful && notAlreadyExplored then 
                { state with 
                    MyExploredCount = state.MyExploredCount + 1    
                    ExploredCount = state.ExploredCount + 1
                }
            else
                state
                
                

            (* let updateState : State -> Percept list -> State *)
        let updateState state percepts = 
            let clearedState = clearTempBeliefs state
            //logImportant (sprintf "%A" (List.filter (fun g -> match g with | JobGoal jg -> true | _ -> false) state.Goals))
            let updatedState = 
                List.fold handlePercept clearedState percepts
                |> updateEdgeCosts state
                |> updateLastPos state
                |> updateProbeCount state
                |> updateExploredCount state

            updatedState
            
        override this.AnalyzePercept(percept, state) = state
            
        override this.FilterIntention((intA, intB)) = Conflictive
