﻿namespace NabfAgentLogic
module Explorer =

    
    open AgentTypes
    open Graphing.Graph
    open Constants
    open Logging
    type ZoneVertex = 
        {   
            Vertex          : Vertex
            ControlValue    : int
            Lock            : bool
            HasAgent        : bool
            Desire          : int
        }

    ///////////////////////////////////Helper functions//////////////////////////////////////
        

    let rec mergeZones (zone:VertexName list) (overlapping:Job list) =
        match overlapping with
        | (_,JobData.OccupyJob(_,oldZone)) :: tail -> mergeZones (List.append oldZone zone) tail
        | _ -> zone


    let rec removeDuplicates (zone:VertexName list) (result:VertexName list) =
        match zone with
        | head :: tail -> 
                        let target = List.tryFind (fun n -> n = head) tail
                        if target.IsSome then removeDuplicates tail result else removeDuplicates tail (head::result)
        | [] -> result

    //Check if any jobs contain the current vertex.
    let checkZoneCandidate (s:State) = 
        let occupyJobs = List.filter (fun (((_,_,jobtype,_),data):Job) -> jobtype = JobType.OccupyJob) s.Jobs
        let l = List.filter (fun ((_,OccupyJob(_,vertices)):Job) -> (List.exists (fun (vn:VertexName) -> s.Self.Node = vn ) vertices)) occupyJobs
        l <> []

    let hasExploredPhase1 (s:State) = (float s.MyExploredCount) > ( EXPLORE_FACTOR_LIGHT * (float s.TotalNodeCount) )

    let onHighValueNode (s:State) = s.World.[s.Self.Node].Value.IsSome && s.World.[s.Self.Node].Value.Value >= ZONE_ORIGIN_VALUE

    let nodePartOfZone (s:State) =
        let occupyJobs = (List.filter (fun ((_,_,jType,_),_) -> jType = JobType.OccupyJob) s.Jobs)
        let occupyJobSet = Set.ofList (List.concat (List.map (fun (_,OccupyJob(l,_)) -> l) occupyJobs))
        Set.contains s.Self.Node occupyJobSet

    let nodeHostile (s:State) = false // Not implemented yet!

    let newZoneFound (s:State) = (onHighValueNode s) && not (nodePartOfZone s) && not (nodeHostile s)

    let hasValueHigherThan node value (s:State) = s.World.[node].Value.IsSome && s.World.[node].Value.Value >= value

    //Find a zone after it has been explored
    let rec findZone explored frontier (s:State) = 
        if Set.isEmpty frontier then explored else
        let node = Set.maxElement frontier
        let newFrontier = Set.remove node frontier
        if (not (Set.contains node explored)) && (hasValueHigherThan node ZONE_BORDER_VALUE s)
        then
            let newExplored = Set.add node explored
            findZone newExplored newFrontier s
        else
            findZone explored newFrontier s 

    //Find a zone To Explore
    let rec zoneToExplore  (s:State) (explore,frontier) = 
        if Set.isEmpty frontier then explore else
        let node = Set.maxElement frontier
        let newFrontier = Set.remove node frontier
        let newExplore = Set.add node explore
        if hasValueHigherThan node ZONE_BORDER_VALUE s then
            let neighbours = Set.difference (Set.map (fun (_,st) -> st) s.World.[node].Edges) explore
            let realFrontier = Set.union neighbours newFrontier
            zoneToExplore s (newExplore,realFrontier)
        else
            zoneToExplore s (newExplore,newFrontier)


    //Recursively check if a zone has been explored
    let rec isDoneExploring node explored (s:State) =
        let neighbourSet = Set.map (fun (_,st) -> st) s.World.[node].Edges
        let newExplored = Set.add s.Self.Node explored
        //Returns: (is not valuable) or (is valuable && has no neighbours that are neither done or in the explored set)
        (not (hasValueHigherThan node ZONE_BORDER_VALUE s)) || (hasValueHigherThan node ZONE_BORDER_VALUE s && not (Set.exists (fun n -> not (Set.contains n explored) && not (isDoneExploring n newExplored s)) neighbourSet))
        

    let rec getOverlappingVertices (vl:VertexName list) (zone:VertexName List) =
        match (vl, zone) with
        | ([], _) -> []
        | (head :: tail, l) -> 
            if (List.tryFind (fun n -> n = head) l).IsNone then 
                head :: (getOverlappingVertices tail l)
            else 
                getOverlappingVertices tail l

    let getOverlappingJobs (occupyJobs:Job list) (zone:VertexName List) =
        List.filter (fun ((_,OccupyJob(_,vertices)):Job) -> (getOverlappingVertices vertices zone) <> []) occupyJobs

    let buildZoneVertex (vertex:Vertex) =
        {
            Vertex = vertex
            ControlValue = 2 + vertex.Edges.Count
            Desire = 0
            Lock = false
            HasAgent = true
        } : ZoneVertex

    let isIsland (vertex:Vertex) =
        vertex.Edges.Count = 1

    //We always want to place an agent at the entrance to an island, so we lock that vertex.
    let shouldZoneVertexLockBasedOnIsland (graph:Graph) (vertex:ZoneVertex) =
        let neighbours = getNeighbours vertex.Vertex.Identifier graph
        List.exists isIsland neighbours
    
    //Get all neighbours that are also in the zone
    let getZoneVertexNeighbour (graph:Graph) (zone:ZoneVertex list) (vertex:ZoneVertex) =
        let neighbours = getNeighbours vertex.Vertex.Identifier graph
        List.filter (fun zoneVertex -> (List.exists (fun vertex -> vertex.Identifier = zoneVertex.Vertex.Identifier) neighbours)) zone
    
    //A vertex should be locked if it has an agent but its or its neighbour's control value is too low for that agent to be removed.
    let shouldZoneVertexLock (graph:Graph) (zone:ZoneVertex list) (vertex:ZoneVertex) =
        if ((vertex.ControlValue <= 3 && not (isIsland vertex.Vertex)) || vertex.Lock) && (vertex.HasAgent) then true
        else //does the vertex have a neighbour that we would lose control of?
            let neighbours = getZoneVertexNeighbour graph zone vertex
            List.exists (fun zn -> zn.ControlValue = 2) neighbours

    let calcControlValue (graph:Graph) (zone:ZoneVertex list) (vertex:ZoneVertex) =
        let init = if vertex.HasAgent then 2 else 0
        let neighbours = getZoneVertexNeighbour graph zone vertex 
        init + List.length (List.filter (fun zn -> zn.HasAgent) neighbours)
    
    // Calculate how much we want to remove the agent on a given vertex
    let calcDesire (graph:Graph) (zone:ZoneVertex list) (vertex:ZoneVertex) =       
        let neighbours = getZoneVertexNeighbour graph zone vertex
        //IF Even -1; IF Uneven +1
        let calcSingleDesire controlValue = (((controlValue % 2) * 2) - 1)
        List.sum (List.map (fun zn -> calcSingleDesire zn.ControlValue ) neighbours)
    
    let hasLockedNeighbour (graph:Graph) (zone:ZoneVertex list) (vertex:ZoneVertex) =
        let neighbours = getZoneVertexNeighbour graph zone vertex
        List.exists (fun zn -> zn.Lock ) neighbours

    //Choose a vertex that should have its agent removed
    let chooseRemove (graph:Graph) (zone:ZoneVertex list)= 
        let sortedZone = List.rev (List.sortBy (fun zv -> zv.Desire) zone)
        let allLocked = List.forall (fun zv -> zv.Lock) zone
        if allLocked || List.isEmpty sortedZone then //no more agents can be removed
            None
        else //remove an agent
            //First try to find a vertex with a locked neighbour which has an agent and is not locked itself
            let vertexWithLockedNeighbour = List.tryFind ( fun zn -> (hasLockedNeighbour graph zone zn) && (not zn.Lock) && (zn.HasAgent)) sortedZone
            if vertexWithLockedNeighbour.IsSome then
                vertexWithLockedNeighbour
            else //find a vertex which has an agent and is not locked itself
                List.tryFind (fun zn -> (not zn.Lock) && (zn.HasAgent) ) sortedZone
                
    //Recursively remove agents from a zone for as long as we still control it
    let rec calcAgentPositions (graph:Graph) (zone:ZoneVertex list) =
        let zoneWithControlValuesUpdated = List.map (fun zoneVertex -> {zoneVertex with ControlValue = calcControlValue graph zone zoneVertex}) zone
        let zoneWithVerticesLocked = List.map (fun zoneVertex -> {zoneVertex with Lock = (shouldZoneVertexLock graph zoneWithControlValuesUpdated zoneVertex)}) zoneWithControlValuesUpdated
        let zoneWithDesiresUpdated = List.map (fun zoneVertex -> {zoneVertex with Desire = calcDesire graph zoneWithVerticesLocked zoneVertex}) zoneWithVerticesLocked
        let agentToRemove = chooseRemove graph zoneWithDesiresUpdated
        match agentToRemove with
        | None -> zoneWithDesiresUpdated
        | Some removed -> 
            let zoneWithAgentRemoved =    {   removed with HasAgent = false}
                                          ::  (List.filter (fun zn -> zn.Vertex.Identifier <> removed.Vertex.Identifier ) zoneWithDesiresUpdated)
            calcAgentPositions graph zoneWithAgentRemoved

    //Find out where agents should be placed. This is the main function to be called by the logic.
    let findAgentPlacement (subgraph:Vertex List) (graph:Graph) =
        let zoneVertexList = List.map buildZoneVertex subgraph
        let lockedZoneVertexList = List.map (fun zoneVertex -> {zoneVertex with Lock = (shouldZoneVertexLockBasedOnIsland graph zoneVertex); HasAgent = not (isIsland zoneVertex.Vertex)}) zoneVertexList
        let agentPositionsCalculated = calcAgentPositions graph lockedZoneVertexList
        let agentPositions = List.filter (fun zoneVertex -> zoneVertex.HasAgent ) agentPositionsCalculated
        List.map (fun zoneVertex -> zoneVertex.Vertex.Identifier) agentPositions

    let calcZoneValue  (state:State) (agents:int) (zone:string Set) =
        let hasEnemy = Set.exists (fun name -> List.exists (fun agent -> agent.Node = name) state.EnemyData) zone
        let fullvalue = Set.fold (fun value name -> if state.World.[name].Value = None then value else value + state.World.[name].Value.Value) 0 zone
        if hasEnemy then
            fullvalue / 2
        else
            fullvalue
    
    ////////////////////////////////////////Logic////////////////////////////////////////////

    let findNewZone (inputState:State) = 
        if (newZoneFound inputState)
        then
            let origin = inputState.Self.Node
            Some("probe a new zone.",Activity,[
                                               MultiRequirement(
                                                        fun state -> 
                                                            
                                                                          let zone = zoneToExplore state (Set.empty,Set [origin])
                                                                          let vals = List.map (fun z -> (z,state.World.[z].Value)) <| Set.toList zone
                                                                          let withVal = List.filter (fun (_,value) -> Option.isSome value) vals
                                                                          List.map (fun z -> (fun st -> state.World.[z].Value.IsSome)) <| Set.toList zone
                                                                          //Set.forall (fun z -> state.World.[z].Value.IsSome) zone
//                                                                        isDoneExploring origin Set.empty state
                                                        ); 
                                               Plan(fun state ->
                                                    let exploredZone = zoneToExplore state (Set.empty,Set [origin])
                                                    let zone = Set.filter (fun z -> hasValueHigherThan z ZONE_BORDER_VALUE state) exploredZone
                                                    //let zone = findZone Set.empty (Set [origin]) state
                                                    let overlapping = getOverlappingJobs state.Jobs (Set.toList zone)
                                                    match overlapping with
                                                    | [] -> 
                                                        let subGraph = List.map (fun n -> state.World.[n]) (Set.toList zone)
                                                        let agentPositions = findAgentPlacement subGraph state.World
                                                        let agentsNeeded = agentPositions.Length
                                                        let zoneValue = calcZoneValue state agentsNeeded zone
                                                        [Communicate( CreateJob( (None,zoneValue,JobType.OccupyJob,agentsNeeded),OccupyJob(agentPositions,Set.toList zone) ) )]
                                                    | head::tail -> 
                                                        let removeIds = List.map ( fun ((id:Option<JobID>,_,_,_),_) -> if id.IsSome then id.Value else -1) overlapping
                                                        let merged = removeDuplicates (mergeZones (Set.toList zone) overlapping) []
                                                        let subGraph = List.map (fun n -> state.World.[n]) merged
                                                        let agentPositions = findAgentPlacement subGraph state.World
                                                        let agentsNeeded = agentPositions.Length
                                                        let zoneValue = calcZoneValue state agentsNeeded (Set.ofList merged)
                                                        let addZone = [Communicate( CreateJob( (None,zoneValue,JobType.OccupyJob,agentsNeeded),OccupyJob(agentPositions,merged) ) )] 
                                                        let removeZones = List.map (fun id -> Communicate(RemoveJob(id))) removeIds
                                                        List.concat [removeZones;addZone]
                                               )])
        else
            None

    let applyToOccupyJob (s:State) = None


    let findNodeToProbeUnconditional (s:State) = 
        if s.ProbedCount < s.TotalNodeCount
            then
                Some("probe one more node.",Activity,[Requirement(fun state -> match state.LastAction with 
                                                                                                       | Probe _ -> true
                                                                                                       | _ -> false
                                                                                                       )])
            else
                None

    let findNodeToProbePhase1 (s:State) = if(hasExploredPhase1 s) then findNodeToProbeUnconditional s else None