namespace NabfAgentLogic
module HandlePercepts =
    open FsPlanning.Agent
    open FsPlanning.Agent.Planning
    open AgentTypes
    open Graphing.Graph
    open NabfAgentLogic.Logging
    open NabfAgentLogic.LogicLib
    open NabfAgentLogic.Search.HeuristicDijkstra
    open Constants
    open GeneralLib

    ///////////////////////////////////Helper functions//////////////////////////////////////

    

    let rec addListToMap mapToUpdate list =
        match list with 
        | (key,value)::tail -> 
                                let newMap = Map.add key value mapToUpdate
                                addListToMap newMap tail
        | [] -> mapToUpdate

    let rec addListOfMapsToMap mapToUpdate list =
        match list with 
        | head::tail -> 
                        let newMap = addListToMap mapToUpdate (Map.toList head)
                        addListOfMapsToMap newMap tail
        | [] -> mapToUpdate


    ///////////////////////////////End of Helper functions///////////////////////////////////

(* handlePercept State -> Percept -> State *)
    let handlePercept state percept =
        match percept with
            | EnemySeen enemy when enemy.Name <> state.Self.Name ->
                let updateAgentData agentNew agentOld = 
                    let oldUpdated = match agentNew.Role with
                                        | Some role -> { agentOld with Role = agentNew.Role }
                                        | None -> agentOld
                        
                    { oldUpdated with Node = agentNew.Node 
                                      Team = agentNew.Team
                                      Status = agentNew.Status 
                                      RoleCertainty = agentNew.RoleCertainty
                    }       
                        
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
            
            | EnemySeen _ -> state
                
            | VertexSeen seenVertex ->
//                { state with NewVertices = seenVertex::state.NewVertices }
                match seenVertex with
                | (nodeName,Some team) when team <> OUR_TEAM -> 
                    { state with NodesControlledByEnemy = Set.add nodeName state.NodesControlledByEnemy }
                | _ -> state

            | VertexProbed (name, value) ->
                logStateImportant state Perception <| sprintf "vertex probed percept %A %A" name value
                { state with 
                        World = addVertexValue name value state.World
                }
                        
            | EdgeSeen (cost, node1, node2) ->
                let edgeAlreadyExistsWithValue = fun (cost':Option<_>, otherVertexId) -> cost'.IsSome && otherVertexId = node2

                let containNode = (Map.containsKey node1 state.World)
                //let edges = state.World.[node1].Edges
                //logInfo ("Contains Node: "+containNode.ToString())
                if ( cost.IsNone && not (containNode && (Set.exists edgeAlreadyExistsWithValue state.World.[node1].Edges))) then
                    //printf "\n Added new edge from %A to %A with cost %A to state \n" node1 node2 cost
                    { state with 
                        World = addEdge (cost, node1, node2) state.World 
                        NewEdges = (cost, node1, node2) :: state.NewEdges
                    }      
                elif ( cost.IsSome ) then
                    //printf "\n Added new edge from %A to %A with cost %A to state \n" node1 node2 cost
                    { state with 
                        World = addEdge (cost, node1, node2) state.World 
                        NewEdges = (cost, node1, node2) :: state.NewEdges
                    }                
                else
                    state

                
            | SimulationStep step  -> { state with SimulationStep = step }
            | MaxEnergyDisabled energy -> state //prob not needed, part of Self percept
            | LastAction action    ->  { state with LastAction = action }

            | LastActionResult res -> { state with LastActionResult = res }
            | ZoneScore score      -> { state with ThisZoneScore = score }
            | Team team ->
                { state with 
                    TeamZoneScore = team.ZoneScore
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
                    else addNothing name role
                                            
                match agentRole with
                | (name, role, certainty) -> if certainty = 100 then addAgentRole name (Some role)
                                                else state// We might want to add functionality for certainty < 100%
           
            | JobPercept job -> 
                let jobIDFromHeader (header:JobHeader) =
                        match header with
                        | (jobID, _, _, _) -> jobID

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
                    //logImportant <| sprintf "Added job w. id %A to myjobs. Round is %A" jobID state.SimulationStep
                    if not <| List.exists ((=) (jobID,vertexName)) state.MyJobs 
                    then
                        { state with MyJobs = (jobID, vertexName)::state.MyJobs }
                    else
                        state

                | FiredFrom jobID -> 
                    logStateInfo state Perception <| sprintf "received fired from job with id %A" jobID
                    let existingJobRemoved = removeMyJob jobID

                    { state with MyJobs =  existingJobRemoved }

               
//            | KnowledgeSent pl -> 
//                    let updatedNK = List.filter (fun p -> not <| List.exists ((=) p) pl) state.NewKnowledge
//                    logImportant <| sprintf "Clearing knowledge sent. We sent %A knowledge" pl.Length
//                    { state with NewKnowledge = updatedNK }

            | HeuristicUpdate (n1,n2,dist) -> 
                let (heuMap,countMap) = state.GraphHeuristic 
                {state with GraphHeuristic = (Map.add (n1,n2) dist heuMap,countMap)}
            | MailPercept mail ->
                let roundNr = state.SimulationStep
                let newMails = 
                    match Map.tryFind roundNr state.MailsReceived with
                    | Some oldMails -> Set.add mail oldMails
                    | None -> Set [mail]
                { state with MailsReceived = Map.add roundNr newMails state.MailsReceived }
            | CommucationSent cs -> 
                match cs with
                | ShareKnowledge pl -> 
                    let updatedNK = List.filter (fun p -> not <| List.exists ((=) p) pl) state.NewKnowledge
                    //logImportant <| sprintf "Clearing knowledge sent. We sent %A knowledge" pl.Length
                    { state with NewKnowledge = updatedNK } 
                | UnapplyJob jid -> 
                    let removeMyJob jobID = List.filter (fst >> ((=) jobID)) state.MyJobs
                    let existingJobRemoved = removeMyJob jid
                    { state with MyJobs =  existingJobRemoved }
                | _ -> state

             
            | unhandled -> logStateError state Perception (sprintf "Unhandled percept: %A" unhandled) 
                           state //fix this later by handling remaining percepts

            
    let clearTempBeliefs (state:State) =
        let newEnemyData = List.map (fun enemy -> { enemy with Agent.Node = ""}) state.EnemyData
        let newAllyData = List.map (fun ally -> { ally with Status = Disabled; Node = "" }) state.FriendlyData
        { state with 
            NewEdges = []
//            NewVertices = []
            EnemyData = newEnemyData
            FriendlyData = newAllyData
            NodesControlledByEnemy = Set.empty
        }

    let updateTraversedEdgeCost (oldState : State) (newState : State) =
                    match (oldState.Self.Node, newState.LastAction, newState.LastActionResult) with
                    | (fromVertex, Goto toVertex, Successful) -> 
                        
                        let edge = (Some (oldState.Self.Energy.Value - newState.Self.Energy.Value), fromVertex, toVertex)
                        //printf "\n Updated cost on edge %A \n" edge
                        { newState with 
                            World = addEdge edge newState.World
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
        { state with ProbedCount = List.length <| LogicLib.probedVertices state.World }

//    let updateProbeCount (lastState:State) (state:State) =
//            
//        let probeAction = match state.LastAction with
//                            | Action.Probe param -> true
//                            | _ -> false
//        let resultSuccessful = state.LastActionResult = ActionResult.Successful
//        let getProbedVertex = lastState.Self.Node
//        
//        let notAlreadyProbed = lastState.World.ContainsKey getProbedVertex && lastState.World.[getProbedVertex].Value = Option.None
//        if probeAction && resultSuccessful && notAlreadyProbed then 
//
//            { state with   
//                ProbedCount = state.ProbedCount + 1
//            }
//        else
//            state

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
        | VertexSeen (vertexName, ownedBy) -> not (oldState.World.ContainsKey vertexName ) 
        | EdgeSeen (edgeValue, node1, node2) ->
            if oldState.World.ContainsKey(node1) then
                let edge = Set.filter (fun (_, endNode) -> endNode = node2) oldState.World.[node1].Edges
                if edge.Count = 1 then 
                                    match edge.MaximumElement with
                                    | (value, _) -> value.IsNone && edgeValue.IsSome
                elif edge.Count = 0 then true
                else
                    raise(System.Exception("Handle edge seen percept - found "+ string(edge.Count) + " of the given edge in the world."))
            else
                false //Check this, will we ever see an edge before one of its vertices?
          

        | EnemySeen { Role = role ; Name = name} -> 
            match (List.filter (fun a -> a.Name = name) oldState.EnemyData) with
            | head::tail ->                             
                            if (role.IsSome && head.Role.IsNone) then
                                true
                            else
                                false
            | _ -> false
        //false//Should be shared when we learn of the agents role, as well as every time it is spotted!! TODO!!!
//            let agentIsKnown agentData = 
//                match agentData with
//                | { Name = agentDataName ; Role = Some _ } -> agentDataName = name
//                | _ -> false
//            not (List.exists agentIsKnown oldState.EnemyData)

        | AgentRolePercept (name, role, certainty) -> 
            if List.exists (fun (enemyAgent:Agent) -> enemyAgent.Name = name) state.EnemyData then 
                true
            else
                false

        | _ -> false


    let selectSharedPercepts percepts oldState (state:State) =
        let propagatedPercepts = List.filter (shouldSharePercept oldState state) percepts
        { state with 
                NewKnowledge = state.NewKnowledge @ propagatedPercepts
        }

//    let updateHeuristicsMap percepts oldState state =
//        if state.World.Count > oldState.World.Count then             
//            
//            let result = 
//                
//
//                let nodeNames = List.map fst (Map.toList state.World)
//                let nodeNumbers = List.map (fun (s:string) -> s.Remove(0,1) |> int ) nodeNames  
//                let myNumber = (state.Self.Name.Remove(0, OUR_TEAM.Length) |> int)
//                let nodeNumbersToFindHeuristicFor = List.filter (fun i -> i % NUMBER_OF_AGENTS = myNumber) nodeNumbers
//                let nodeNamesToFindHeuristicFor = List.map (fun i -> "v" + (string i)) nodeNumbersToFindHeuristicFor
//                let heuristics = List.map (fun s -> allDistancesMap state.World s) nodeNamesToFindHeuristicFor
//                let newHeuristicMap = addListOfMapsToMap state.HeuristicMap heuristics
//
//                let difHeus = List.filter 
//                                            (
//                                                fun (key,dist) -> match Map.tryFind key state.HeuristicMap with
//                                                                    | Some (oldDist) -> dist < oldDist
//                                                                    | _ -> true
//                                            ) 
//                                                <| Map.toList newHeuristicMap
//                
//                { state with //UpdateMap = true
//                        //HeuristicMap = allPairsDistances state.World
//                        HeuristicMap = newHeuristicMap
//                        
//                        //NewKnowledge = List.append state.NewKnowledge (List.map (fun ((n1,n2),dist) -> HeuristicUpdate(n1,n2,dist)) difHeus)
//                }
//            
//            result
//        else
//            state

    let updateHeuristicsMapSingle percepts oldState state =
        updateHeuristic state state.Self.Node
  
    let clearOldMails (state:State) = 
        //split mails into new mails and mails that are MAIL_EXPIRATION number of rounds old
        let (fresh,old) = Map.partition (fun mailRound _ ->  (state.SimulationStep - MAIL_EXPIRATION) <= mailRound) state.MailsReceived 
        { state with MailsReceived = fresh }    
    
    let readMail (state:State) ((sender,_,message):Mail) =
        match message with
        | GoingToRepairYou ->
            { state with Relations = Map.add MyRepairer sender state.Relations  }
        | MyLocation vn ->
            let updatedFriendlyList = updateAgentPosition sender vn state.FriendlyData 
            { state with FriendlyData = updatedFriendlyList }     

    let rec inferKnowlegdeMails  mailgroups (state:State) =
        if not <| Map.isEmpty mailgroups then
            let (round,mails) =  Set.minElement <| (Set.ofSeq <| Map.toSeq mailgroups)
            let updatedState = Set.fold readMail state mails
            inferKnowlegdeMails (Map.remove round mailgroups) updatedState
        else
            state

    let rec inferKnowledgeJobs jobs state =
        match jobs with
        | job::rest ->
            let newState = 
                match job with
                | (_,RepairJob(vn,agent)) -> 
                    let updatedFriendlyList = updateAgentPosition agent vn state.FriendlyData 
                    { state with  FriendlyData = updatedFriendlyList }

                | _ -> state
            inferKnowledgeJobs rest newState
        | [] -> state
    
    let removeKnowledgeRelations (s:State) =
        if s.Self.Status = Normal then
            { s with Relations = Map.remove MyRepairer s.Relations }
        else
            s


    (* let updateState : State -> Percept list -> State *)
    let updateState state percepts = 
        let clearedState = clearTempBeliefs state

        let handlePercepts state percepts = List.fold handlePercept state percepts

        let newRoundPercepts s = 
            handlePercepts s percepts
            |> updateLastPos state
            |> updateProbeCount state
            |> updateExploredCount state
            |> updateTraversedEdgeCost state
            |> selectSharedPercepts percepts state
            |> updateHeuristicsMapSingle percepts state
            |> clearOldMails
        
        let inferKnowledge (s:State) = 
            inferKnowledgeJobs s.Jobs s
            |> inferKnowlegdeMails s.MailsReceived                  

        let removeKnowledge s =
            removeKnowledgeRelations s

        match percepts with
        | NewRoundPercept::_ -> 
            let inferedState = inferKnowledge clearedState
            let newState = 
                newRoundPercepts inferedState
                |> removeKnowledge     
            logImportant Perception ("State finished updating now at step " + newState.SimulationStep.ToString())
            newState           
        | _ -> 
            let newState = inferKnowledge state
            handlePercepts newState percepts
            |> removeKnowledge 
        
