namespace NabfAgentLogic
module Explorer =

    
    open AgentTypes
    open Graphing.Graph
    open Constants
    open Logging
    open LogicLib
    open NabfAgentLogic.Search

    type ZoneVertex = 
        {   
            Vertex          : Vertex
            ControlValue    : int
            Lock            : bool
            HasAgent        : bool
            Desire          : int
        }

    ///////////////////////////////////Helper functions//////////////////////////////////////
        
    ///Functions for finding islands///

    //Check if this is an articulation point
    let articulationPoint components = (List.length components) > 1

    //Filter away unexplored components
    let getExploredComponents (s:State) components = List.filter (fun nodeSet -> Set.forall (fun name -> s.World.ContainsKey name && not <| isUnexplored s name) nodeSet) components

    //Function for filtering away probed islands. Should only be used for that (unsafe).
    let getUnprobedComponents (s:State) components = List.filter (fun nodeSet -> Set.exists (fun name -> s.World.[name].Value.IsNone ) nodeSet) components

    ///////////////////////////////////


    let rec mergeZones (zone:VertexName list) (overlapping:Job list) =
        match overlapping with
        | (_,JobData.OccupyJob(_,oldZone)) :: tail -> mergeZones (List.append oldZone zone) tail
        | _ -> zone


    let rec removeDuplicatesRec (zone:VertexName list) (result:VertexName list) =
        match zone with
        | head :: tail -> 
                        let target = List.tryFind (fun n -> n = head) tail
                        if target.IsSome then removeDuplicatesRec tail result else removeDuplicatesRec tail (head::result)
        | [] -> result

    let removeDuplicates zone = removeDuplicatesRec zone []

    //Check if any jobs contain the current vertex.
    let checkZoneCandidate (s:State) = 
        let occupyJobs = List.filter (fun (((_,_,jobtype,_),data):Job) -> jobtype = JobType.OccupyJob) s.Jobs
        let l = List.filter (fun ((_,OccupyJob(_,vertices)):Job) -> (List.exists (fun (vn:VertexName) -> s.Self.Node = vn ) vertices)) occupyJobs
        l <> []

    let lightProbingDone (s:State) = (float s.ProbedCount) > ( PROBE_FACTOR_LIGHT * (float s.TotalNodeCount) )

    let onHighValueNode (s:State) = s.World.[s.Self.Node].Value.IsSome && s.World.[s.Self.Node].Value.Value >= ZONE_ORIGIN_VALUE

    let nodePartOfZone (s:State) =
        let occupyJobs = (List.filter (fun ((_,_,jType,_),_) -> jType = JobType.OccupyJob) s.Jobs)
        let occupyJobSet = Set.ofList (List.concat (List.map (fun (_,OccupyJob(_,l)) -> l) occupyJobs))
//        logCritical <| sprintf "%A" s.Jobs
//        logCritical <| sprintf "%A" occupyJobs
//        logCritical <| sprintf "%A" occupyJobSet
//        logCritical <| sprintf "%A" (Set.contains s.Self.Node occupyJobSet)
        Set.contains s.Self.Node occupyJobSet

    let nodeHostile (s:State) = false // Not implemented yet!

    let newZoneFound (s:State) = (onHighValueNode s) && (not (nodePartOfZone s)) && (not (nodeHostile s))

    let hasValueHigherThan node value (s:State) = s.World.[node].Value.IsSome && s.World.[node].Value.Value >= value
    
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
            if (List.tryFind (fun n -> n = head) l).IsSome then 
                head :: (getOverlappingVertices tail l)
            else 
                getOverlappingVertices tail l


    let getOverlappingOccupyJobs (jobs:Job list) (zone:VertexName List) =
        List.filter (fun job -> 
                            match job with 
                            | (_,OccupyJob(_,vertices)) -> (getOverlappingVertices vertices zone) <> []
                            | _ -> false
                        ) jobs

    let buildZoneVertex (vertex:Vertex) =
        {
            Vertex = vertex
            ControlValue = 2 + vertex.Edges.Count
            Desire = 0
            Lock = false
            HasAgent = true
        } : ZoneVertex
    
    let isProbed (vertexName:VertexName) (world:Graph) = world.ContainsKey vertexName &&  world.[vertexName].Value.IsSome
           


    let isIsland (vertex:Vertex) =
        vertex.Edges.Count = 1

    let isOnlyIsland (graph:Graph) (zone:ZoneVertex list) =
        zone.Length = 1 && isIsland zone.Head.Vertex

    let getValue (v:Vertex) =
        match v.Value with
        | Some value -> value
        | None -> 0

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
        if isOnlyIsland graph zoneVertexList 
        then 
            let (opt,neighbour) = subgraph.Head.Edges.MaximumElement
            [neighbour] 
        else
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
    
    let nodeIsUnprobed (state:State) node =
        let n = state.World.[node] 
        n.Value.IsNone

    let createIslandObjectives unprobedIslands = 
        match unprobedIslands with
        | island :: tail -> [MultiGoal( fun state -> List.map (Probed) <| Set.toList island);
                                  Plan(fun state ->
                                       let subGraph = List.map (fun n -> state.World.[n]) (Set.toList island)
                                       let positions = findAgentPlacement subGraph state.World
                                       let agentsNeeded = positions.Length
                                       let value = calcZoneValue state agentsNeeded island
                                       let islandAsList = Set.toList island
                                       Some [Communicate( CreateJob( (None,value,JobType.OccupyJob,agentsNeeded),OccupyJob(positions,islandAsList) ) )]
                                    )]
        | [] -> []

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let findNewZone (inputState:State) = 
        if (newZoneFound inputState)
        then
            
            let origin = inputState.Self.Node
            Some<| normalIntention 
                (   "probe a new zone.",
                    Activity,
                    [ MultiGoal(
                                fun state -> 
                                let zone = zoneToExplore state (Set.empty,Set [origin])
                                List.map (Probed) <| Set.toList zone
                            ); 
                   Plan(fun state ->
                        let exploredZone = zoneToExplore state (Set.empty,Set [origin])
                        let zone = Set.filter (fun z -> hasValueHigherThan z ZONE_BORDER_VALUE state) exploredZone
                        //let zone = findZone Set.empty (Set [origin]) state
                        let overlapping = getOverlappingOccupyJobs state.Jobs (Set.toList zone)
                        match overlapping with
                        | [] -> 
                            let subGraph = List.map (fun n -> state.World.[n]) (Set.toList zone)
                            let agentPositions = findAgentPlacement subGraph state.World
                            let agentsNeeded = agentPositions.Length
                            let zoneValue = calcZoneValue state agentsNeeded zone
                            Some [Communicate( CreateJob( (None,zoneValue,JobType.OccupyJob,agentsNeeded),OccupyJob(agentPositions,Set.toList zone) ) )]
                        | head::tail -> 
                            let removeIds = List.map ( fun ((id:Option<JobID>,_,_,_),_) -> if id.IsSome then id.Value else -1) overlapping
                            let merged = removeDuplicates (mergeZones (Set.toList zone) overlapping)
                            let subGraph = List.map (fun n -> state.World.[n]) merged
                            let agentPositions = findAgentPlacement subGraph state.World
                            let agentsNeeded = agentPositions.Length
                            let zoneValue = calcZoneValue state agentsNeeded (Set.ofList merged)
                            let addZone = [Communicate( CreateJob( (None,zoneValue,JobType.OccupyJob,agentsNeeded),OccupyJob(agentPositions,merged) ) )] 
                            let removeZones = List.map (fun id -> Communicate(RemoveJob(id))) removeIds
                            Some (List.concat [removeZones; addZone])
                   )])
        else
            None

    let findNewIslandZone (inputState:State) =
        let biconnectedComponents = Biconnected.find inputState.Self.Node inputState.World
        let possibleIslands = List.tail <| List.rev (List.sortBy Set.count biconnectedComponents)
        if articulationPoint biconnectedComponents 
        then
                let knownIslands = getExploredComponents inputState possibleIslands
                let unprobedIslands = getUnprobedComponents inputState knownIslands
                if unprobedIslands.Length = 0 
                then
                    Some <| normalIntention (
                            sprintf "probe one of %A islands." unprobedIslands.Length, 
                            Activity, 
                            createIslandObjectives unprobedIslands)
                else None
        else None
     
       

    let findNodeToProbe (inputState:State) =
        let nearestUnprobed = nearestVertexSatisfying inputState nodeIsUnprobed
        if(nodeHasNoOtherFriendlyAgentsOnIt inputState inputState.Self.Node)
        then
            match nearestUnprobed with
            | Some unprobed ->
                Some <| normalIntention ("probe one more node.", Activity, [Requirement (Probed unprobed)])
            | _ -> None
        else
            let otherAgentsOnMyNode = List.filter (fun a -> a.Node = inputState.Self.Node && not(a.Name = inputState.Self.Name)) inputState.FriendlyData
            if (myRankIsGreatest inputState.Self.Name otherAgentsOnMyNode)
            then
                let nextBest = findNextBestUnprobed inputState
                match nextBest with
                        | Some vertex -> Some<| normalIntention ("leave the group and probe a node.", Activity, [Requirement (Probed vertex)])
                        | None -> Some<| normalIntention ("wait for the others to leave.", Activity, [Plan (fun _ -> Some [Perform(Recharge)])])
            else
                match nearestUnprobed with
                | Some unprobed ->
                    Some<| normalIntention ("probe one more node.", Activity, [Requirement (Probed unprobed)])
                | _ -> None