namespace NabfAgentLogic
module HandlePercepts =
    open FsPlanning.Agent
    open FsPlanning.Agent.Planning
    open AgentTypes
    open Graphing.Graph
    open NabfAgentLogic.Logging

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
                
            | VertexSeen seenVertex -> 
                { state with NewVertices = seenVertex::state.NewVertices} 

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

                
            | SimulationStep step  -> { state with SimulationStep = step }
            | MaxEnergyDisabled energy -> state //prob not needed, part of Self percept
            | LastAction action    -> { state with LastAction = action }
            | LastActionResult res -> { state with LastActionResult = res }
            | ZoneScore score      -> { state with ThisZoneScore = score }
            | Team team ->
                { state with 
                    TeamZoneScore = team.ZoneScore
                    Money = team.Money
                    LastStepScore = team.LastStepScore
                    Score = team.Score
                }
            | Self self ->
                    
                let newSelf = { self with 
                                    Name = state.Self.Name
                                    Team = state.Self.Team
                                    Role = state.Self.Role
                }
                let newSelfDisabled = { self with 
                                            Name = state.Self.Name
                                            Team = state.Self.Team
                                            Role = state.Self.Role
                                            MaxEnergy = self.MaxEnergyDisabled
                }
                match self.Status with
                | EntityStatus.Disabled -> { state with Self = newSelfDisabled }
                | _ -> { state with Self = newSelf }
                    
                    
            | NewRoundPercept -> state //is here for simplicity, should not do anything

            | AgentRolePercept agentRole -> 
                         
                let addRole name role (list:Agent list) = 
                    let agent = (List.filter (fun (a:Agent) -> a.Name = name) list).Head
                    let others = List.filter (fun (a:Agent) -> a.Name <> name) list

                    if agent.Role = role then list
                    else 
                        let newAgent = { agent with Role = role}
                        newAgent::others

                let addEnemyRole name role = 
                    let updatedData = addRole name role state.EnemyData
                    { state with EnemyData = updatedData}

                let addFriendlyRole name role = 
                    let updatedData = addRole name role state.FriendlyData
                    { state with FriendlyData = updatedData}

                let addNothing (name:string) (role:Option<AgentRole>) =
                    state

                let addAgentRole name (role:Option<AgentRole>) = 
                    if (List.exists (fun (a:Agent) -> a.Name = name) state.EnemyData) then addEnemyRole name role
                    elif (List.exists (fun (a:Agent) -> a.Name = name) state.FriendlyData) then addFriendlyRole name role
                    else addNothing name role //Fix this!! TODO!!!!
                                            
                match agentRole with
                | (name, role, certainty) -> if certainty = 100 then addAgentRole name (Some role)
                                                else state// We might want to add functionality for certainty < 100%
                | _ -> state

            | JobPercept job -> 
                let jobIDFromHeader (header:JobHeader) =
                        match header with
                        | (jobID, _, _, _) -> jobID
                        | _ -> None

                let removeJob jobHeader = List.filter (fun (existingJobHeader, _) -> 
                            not(jobIDFromHeader existingJobHeader = jobIDFromHeader jobHeader)) state.Jobs

                let removeMyJob jobID = List.filter (fun (existingJobID, _) -> 
                            not(existingJobID = jobID)) state.MyJobs

                match job with 
                | AddedOrChangedJob (jobHeader, jobData) -> 
                    
                    let changeJob = List.exists (fun (existingJobHeader, _) -> 
                        jobIDFromHeader existingJobHeader = jobIDFromHeader jobHeader) state.Jobs

                    let updateJob = 
                        let existingJobRemoved = removeJob jobHeader
                        { state with Jobs =  (jobHeader, jobData)::existingJobRemoved }

                    let addJob = { state with Jobs = (jobHeader, jobData)::state.Jobs }

                    if changeJob then updateJob
                    else addJob

                | RemovedJob (jobHeader, jobData) -> 
                    let existingJobRemoved = removeJob jobHeader

                    { state with Jobs =  existingJobRemoved }

                | AcceptedJob (jobID, vertexName) -> 
                    { state with MyJobs = (jobID, vertexName)::state.MyJobs }

                | FiredFrom jobID -> 
                    let existingJobRemoved = removeMyJob jobID

                    { state with MyJobs =  existingJobRemoved }

                | _ -> state

            | KnowledgeSent pl -> 
                    let updatedNK = List.filter (fun p -> List.exists ((=) p) pl) state.NewKnowledge
                    { state with NewKnowledge = updatedNK }
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
                //NewEdges = edge :: newState.NewEdges 
            }
        | _ -> newState

//    let updateEdgeCosts (lastState:State) (state:State) =
//        match (state.LastAction, state.LastActionResult) with
//        | (Goto _, Successful) ->
//            let state4 = updateTraversedEdgeCost lastState state
//            state4
//        | _ -> updateTraversedEdgeCost lastState state

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
        
        let notAlreadyProbed = lastState.World.ContainsKey getProbedVertex && lastState.World.[getProbedVertex].Value = Option.None
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
        let notAlreadyExplored = lastState.World.ContainsKey getGotoVertex && ( Set.forall (fun (value, _) -> value = Option.None) lastState.World.[getGotoVertex].Edges )

        if gotoAction && resultSuccessful && notAlreadyExplored then 
            { state with 
                MyExploredCount = state.MyExploredCount + 1    
                ExploredCount = state.ExploredCount + 1
            }
        else
            state
                
    let shouldSharePercept oldState (state:State) percept = 
        match percept with
        | VertexProbed (vertexName, value) -> 
            if oldState.World.ContainsKey(vertexName) then 
                let vertex = oldState.World.[vertexName]
                if vertex.Value.IsNone then 
                    true
                else
                    false
            else
                true
        | VertexSeen (vertexName, ownedBy) -> not (oldState.World.ContainsKey(vertexName))
        | EdgeSeen (edgeValue, node1, node2) ->
//            if oldState.World.ContainsKey(node1) then
//                let edge = Set.filter (fun (_, endNode) -> endNode = node2) oldState.World.[node1].Edges
//
//                if edge.Count = 1 then 
//                                    match edge.MaximumElement with
//                                    | (value, _) -> value.IsNone
//                                    | _ -> false
//                else
//                    raise(System.Exception("Handle edge seen percept - found a wrong number of the given edge in the world."))
//            else
//                false
            false

        | EnemySeen { Role = role ; Name = name} -> //Should be shared when we learn of the agents role, as well as every time it is spotted!! TODO!!!
            let agentIsKnown agentData = 
                match agentData with
                | { Name = agentDataName ; Role = Some _ } -> agentDataName = name
                | _ -> false
            not (List.exists agentIsKnown oldState.EnemyData)

        | _ -> false


    let selectSharedPercepts percepts oldState (state:State) =
        let propagatedPercepts = List.filter (shouldSharePercept oldState state) percepts
        { state with 
                NewKnowledge = state.NewKnowledge @ propagatedPercepts
        }
    
    (* let updateState : State -> Percept list -> State *)
    let updateState state percepts = 
        let clearedState = clearTempBeliefs state
        let handlePercepts state percepts = List.fold handlePercept state percepts
        let newRoundPercepts s = handlePercepts s percepts
                                |> updateLastPos state
                                |> updateProbeCount state
                                |> updateExploredCount state
                                |> updateTraversedEdgeCost state
                                |> selectSharedPercepts percepts state

        match percepts with
        | NewRoundPercept::_ -> newRoundPercepts clearedState
        | _ -> handlePercepts state percepts
        
