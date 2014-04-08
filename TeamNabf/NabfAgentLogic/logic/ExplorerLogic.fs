namespace NabfAgentLogic

module ExplorerLogic =

    open AgentTypes
    open AgentLogicLib
    open Graphing.Graph
    open PathFinding
    open Logging
    open Constants

    type ZoneVertex = 
        {   
            Vertex          : Vertex
            ControlValue    : int
            Lock            : bool
            HasAgent        : bool
            Desire          : int
        }

    ////////////////////////////////////////
    // Functions for creating occupy jobs //
    ////////////////////////////////////////

    //Check if a vertex v is part of an existing occupy job
    let rec zoneAlreadyFound (jobs:Job list) (v:string) =
        match jobs with
        | [] -> false
        | (_,OccupyJob (_,coveredVertices)) :: tail -> 
            if List.exists (fun n -> n = v) coveredVertices 
            then 
                true 
            else
                zoneAlreadyFound tail v
        | _ -> failwithf "Incorrect input passed to zoneAlreadyFound"

    //Get all neighbours that are not already in the zone
    let getRelevantNeighbours (s:State) (vn:VertexName) =
        let neighbours = List.map (fun v -> v.Identifier) (getNeighbours vn s.World)
        let neighboursNotInFrontier = List.filter (fun st -> (List.tryFind (fun (st2:VertexName) -> st2 = st) s.NewZoneFrontier).IsNone) neighbours
        List.filter (fun v -> not (Map.containsKey v (fst s.NewZone.Value))) neighboursNotInFrontier

    //Get all vertices in vl that are also in NewZone
    let rec getOverlappingVertices (s:State) (vl:VertexName list) = 
//        logImportant (sprintf "%A" vl)
        match (vl, s.NewZone) with
        | (_, None) ->  
                []
        | ([], _) -> []
        | (head :: tail, Some (g, _)) -> 
            if (Map.containsKey head g) then 
                head :: (getOverlappingVertices s tail)
            else 
                getOverlappingVertices s tail
        
    //Get all jobs that contain one or more vertices in NewZone
    let getOverlappingJobs (s:State) (occupyJobs:Job list) =
        logImportant "getOverLappingJobs"
        List.filter (fun ((_,OccupyJob(_,vertices)):Job) -> (getOverlappingVertices s vertices) <> []) occupyJobs


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

    let shouldZoneVertexLockBasedOnIsland (graph:Graph) (zoneV:ZoneVertex) =
        let vl = getNeighbours zoneV.Vertex.Identifier graph
        List.exists isIsland vl
    
    

    let getZoneVertexNeighbour (graph:Graph) (zl:ZoneVertex list) (zoneV:ZoneVertex) =
        let vl = getNeighbours zoneV.Vertex.Identifier graph
        List.filter (fun zv -> (List.exists (fun v -> v.Identifier = zv.Vertex.Identifier) vl)) zl
    
    let shouldZoneVertexLock (graph:Graph) (zl:ZoneVertex list) (zoneV:ZoneVertex) =
        if ((zoneV.ControlValue <= 3 && not (isIsland zoneV.Vertex)) || zoneV.Lock) && (zoneV.HasAgent) then true
        else
            let zneighbours = getZoneVertexNeighbour graph zl zoneV
            List.exists (fun zn -> zn.ControlValue = 2) zneighbours

    let calcControlValue (graph:Graph) (zl:ZoneVertex list) (zoneV:ZoneVertex) =
        let init = if zoneV.HasAgent then 2 else 0
        let vl = getNeighbours zoneV.Vertex.Identifier graph
        let zbours = getZoneVertexNeighbour graph zl zoneV 
        init + List.length (List.filter (fun zn -> zn.HasAgent) zbours)
    
    let calcDesire (graph:Graph) (zl:ZoneVertex list) (zoneV:ZoneVertex) =       
        let zneighbours = getZoneVertexNeighbour graph zl zoneV
        //IF Even -1; IF Uneven +1
        let calcSingleDesire cval = (((cval % 2) * 2) - 1)
        List.sum (List.map (fun zn -> calcSingleDesire zn.ControlValue ) zneighbours)
    
    let hasLockedNeighbour (graph:Graph) (zl:ZoneVertex list) (zoneV:ZoneVertex) =
        let zneighbours = getZoneVertexNeighbour graph zl zoneV
        List.exists (fun zn -> zn.Lock ) zneighbours

    let choseRemove (graph:Graph) (zl:ZoneVertex list)= 
        let sorted = List.rev (List.sortBy (fun zv -> zv.Desire) zl)
        let allLocked = List.forall (fun zv -> zv.Lock) zl
        if allLocked || List.isEmpty sorted then
            None
        else
            let findNLocked = List.tryFind ( fun zn -> (hasLockedNeighbour graph zl zn) && (not zn.Lock) && (zn.HasAgent)) sorted
            if findNLocked.IsSome then
                findNLocked
            else
                List.tryFind (fun zn -> (not zn.Lock) && (zn.HasAgent) ) sorted
                
    
    let rec calcAgentPositions (graph:Graph) (zl:ZoneVertex list) =
        let zl1 = List.map (fun zn -> {zn with ControlValue = calcControlValue graph zl zn}) zl
        let zl2 = List.map (fun zn -> {zn with Lock = (shouldZoneVertexLock graph zl1 zn)}) zl1
        let zl3 = List.map (fun zn -> {zn with Desire = calcDesire graph zl2 zn}) zl2
        let rz = choseRemove graph zl3
        match rz with
        | None -> zl3
        | Some removed -> 
            let zl4 =    {   removed with HasAgent = false}
                        ::  (List.filter (fun zn -> zn.Vertex.Identifier <> removed.Vertex.Identifier ) zl3)
            calcAgentPositions graph zl4

    let findAgentPlacement (subgraph:Graph) =
        let vl = snd (List.unzip (Map.toList subgraph))
        let zl = List.map buildZoneVertex vl
        let zll = List.map (fun zn -> {zn with Lock = (shouldZoneVertexLockBasedOnIsland subgraph zn); HasAgent = not (isIsland zn.Vertex)}) zl
        let zl = calcAgentPositions subgraph zll
        let zl = List.filter (fun zn -> zn.HasAgent ) zl
        List.map (fun zn -> zn.Vertex.Identifier) zl

    let calcZoneValue  (state:State) (agents:int) (zone:Graph) =
        let hasEnemy = Map.exists (fun vn _ -> List.exists (fun ea -> ea.Node = vn) state.EnemyData) zone
        let fullvalue = Map.fold (fun state key value -> if value.Value = None then state else state + value.Value.Value) 0 zone
        if hasEnemy then
            fullvalue / 2
        else
            fullvalue

    let rec mergeZones (zone:VertexName list) (overlapping:Job list) =
        match overlapping with
        | (_,JobData.OccupyJob(_,oldZone)) :: tail -> mergeZones (List.append oldZone zone) tail
        | _ -> zone

    let rec mergeListIntoGraph (s:State) (graph:Graph) (vertices:VertexName list) =
        match vertices with
        | head :: tail -> mergeListIntoGraph s (addVertex graph s.World.[head]) tail
        | [] -> graph

    let rec listEquals l1 l2 =
        match l1 with
        | [] -> true
        | head :: tail -> if (List.tryFind (fun e -> e = head) l2).IsSome then listEquals tail l2 else false

    let vertexHasEnemy vn (s:State) =
        List.exists (fun e -> e.Node = vn) s.EnemyData
    
    //Number of enemies that are neighbours to vn
    let vertexEnemyNeighbours vn (s:State) =
        let neighs = getNeighbourIds vn s.World
        List.length <| List.filter (fun neigh -> vertexHasEnemy neigh s  ) neighs

    //Check if you need to make a new occupy job based on NewZone
    let generateOccupyJobExplorer (s:State) (knownJobs:Job list) =
        
        match s.NewZone with
        | Some (g,true) -> 
            //Remove all nodes with an enemy or has atleast 2 neighbouring  enemies
            match (Map.filter (fun vn _ -> (vertexEnemyNeighbours vn s) < 2 && not (vertexHasEnemy vn s)  ) g) with
            | g' when not <| Map.isEmpty g' -> 
                let zone = List.map (fun v -> v.Identifier) (snd (List.unzip (Map.toList g')))
                logImportant "generateOccupyJobExplorer" 
                let overlapping = getOverlappingJobs s (List.filter (fun ((_,_,jType,_),_) -> jType = JobType.OccupyJob) knownJobs)
                if (List.tryFind (fun (_,JobData.OccupyJob(_,verts)) ->  (listEquals verts zone)) overlapping ).IsSome then ([],[]) //If the job is already in the job list
                elif overlapping = [] //Nothing is overlapping
                then
                  
                    let isSingleIsland (g:Map<_,Vertex>) = g.Count = 1 && (Map.exists(fun _ v -> v.Edges.Count = 1) g)
                    let (zonePoints,finalZone,finalZoneGraph) = 
                        if isSingleIsland g'  then
                            let [(vn,v)] = Map.toList g'
                            let [(_,edgeName)] = Set.toList v.Edges
                            let nzoneGraph = addVertexById edgeName g'
                            ([edgeName],edgeName::zone,nzoneGraph)
                        else
                            ((findAgentPlacement g'),zone,g')
                

                    ([((None,(calcZoneValue s zonePoints.Length finalZoneGraph),JobType.OccupyJob,(List.length zonePoints)),JobData.OccupyJob(zonePoints,finalZone))],[])
                else //There is a conflict with at least one other occupy job. We need to merge.                
                    let mergedZone = mergeZones zone overlapping
                    let newGraph = mergeListIntoGraph s Map.empty<string,Vertex> mergedZone
                    let zonePoints = findAgentPlacement newGraph
                    let removeIDs = List.map (fun (((id,_,_,_),_):Job) -> if id = None then 0 else id.Value) overlapping
                    ([((None,((calcZoneValue s zonePoints.Length (fst s.NewZone.Value))/List.length zonePoints),JobType.OccupyJob,(List.length zonePoints)),JobData.OccupyJob(zonePoints,mergedZone))],removeIDs)

            | _ -> ([],[])
        | _ -> ([],[])


    //////////////////////
    // Behavioral logic //
    //////////////////////

    //Run away from enemy saboteurs. This has lower priority than inspecting.
    let explorerReact (s:State) (agents:Agent list * Agent list) =
        let enemySabs = List.filter (fun a -> (a.Role = Some Saboteur) || (a.Role = None)) (fst agents)
        runAway s enemySabs

    // If the explorer is on an unprobed vertex, probe it.
    let probeVertex (s:State) =
        let rank = rankByType s
        //logInfo ("Vertex has value: "+s.World.[s.Self.Node].Value.IsNone.ToString())
        if s.World.[s.Self.Node].Value.IsNone && rank = 0 then tryDo (Probe None) s else (false,None) //Only one explorer should probe

    //If the explorer is in the middle of finding a new zone to post, keep exploring it. Has lower priority than probe.
    let exploreNewZone (s:State) =
        match s.NewZoneFrontier with
        | head :: _ -> 
            
            let path = pathToNearest s.Self (fun v -> List.exists (fun zV -> zV = v) s.NewZoneFrontier) s.World
                        //pathTo s.Self s.NewZoneFrontier.Head s.World
            
            if path.IsSome && not path.Value.IsEmpty  then
                logInfo ("Next zone node: "+(List.rev path.Value).Head)
                tryGo (s.World.[path.Value.Head]) s
            else
                (false,None)
        | [] -> (false,None)


    //////////////////////////////////////
    // Functions for updating the state //
    //////////////////////////////////////

    let partitionFrontier (s:State) = List.partition (fun vn -> s.World.ContainsKey vn && s.World.[vn].Value.IsSome) s.NewZoneFrontier
        
    let partitionExploredVertices (s:State) (f:VertexName list) = List.partition (fun vn -> s.World.ContainsKey vn && s.World.[vn].Value.IsSome && s.World.[vn].Value.Value >= ZONE_BORDER_VALUE) f

    let rec listUnion l1 l2 = 
        match l1 with
        | [] -> l2
        | head :: tail -> if (List.tryFind (fun e -> e = head) l2).IsNone then listUnion tail (head::l2) else listUnion tail l2

    let rec addToGraph (s:State) l g = 
        match l with
        | [] -> g
        | head :: tail -> addToGraph s tail (addVertex g s.World.[head])

    let rec verticesToAdd (s:State) (l:VertexName list) =
        match l with
        | [] -> []
        | head :: tail -> listUnion (getRelevantNeighbours s head) (verticesToAdd s tail)

    // If you are in the middle of exploring a zone, update NewZone and NewZoneFrontier
    let updateExploreZone (s:State) = 
        match s.NewZone with
        | Some (_,false) ->
            let pFrontier = partitionFrontier s
            let pExplored = partitionExploredVertices s (fst pFrontier)
            let newVertices = verticesToAdd s (fst pExplored)
            let newFrontier = listUnion (snd pFrontier) newVertices
            match newFrontier with
            | [] -> {s with NewZone = Some ((addToGraph s (fst pExplored) (fst s.NewZone.Value)),true); NewZoneFrontier = []}
            | _  -> {s with NewZone = Some ((addToGraph s (fst pExplored) (fst s.NewZone.Value)),false); NewZoneFrontier = newFrontier}
        | _ -> {s with NewZone = None}

    // If you are on a node with value 10, check if it is part of an occupy job, and if not, start exploring the area.
    let findNewZone (s:State) =
        let hasNoNewZone = s.NewZone.IsNone
        let node = s.World.[s.Self.Node].Value
        let occupyJobs = (List.filter (fun ((_,_,jType,_),_) -> jType = JobType.OccupyJob) s.Jobs)
       
        let zoneExists node = zoneAlreadyFound occupyJobs node

        match s.NewZone with
        | Some (g,_) -> 
            logInfo ("Working on: \""+(sprintf "%A" g))
            if Map.exists (fun name _ -> zoneExists(name)) g then { s with NewZone = None; NewZoneFrontier = [] } else s
        | None -> if node.IsSome && node.Value >= ZONE_ORIGIN_VALUE  && not (zoneExists s.Self.Node) 
                    then 
                        logInfo ("Found Job on: \""+(sprintf "%A" s.Self.Node))
                        let newS = {s with NewZone = Some ((Map.add s.Self.Node s.World.[s.Self.Node] Map.empty),false) }
                        {newS with NewZoneFrontier = getRelevantNeighbours newS s.Self.Node } 
                    else s

//        if s.NewZone.IsSome  then
//            s
//        elif s.NewZone.IsNone then 
//            if node.IsSome && node.Value >= ZONE_ORIGIN_VALUE && not (zoneExists s.Self.Node) then
//                let newS = {s with NewZone = Some ((Map.add s.Self.Node s.World.[s.Self.Node] Map.empty),false) }
//                {newS with NewZoneFrontier = getRelevantNeighbours newS s.Self.Node }
//            else
//                s
//        else
//            { s with NewZone = None }

    let updateStateExplorer (s:State) =
        let s2 = findNewZone s
        updateExploreZone s2
   
   /////////////////////////////////////
   ///  DECIDE JOBS
   /////////////////////////////////////

    let decideJobExplore (s:State) (job:Job) =  
        let mapExplored = not ( Map.exists ( fun vn v -> v.Value.IsNone) s.World )
        let noJob = not ( List.exists (fun goal ->   match goal with 
                                                | JobGoal (OccupyGoal _) -> true 
                                                | _ -> false ) s.Goals)
        if mapExplored && noJob then
            match job with
            | ((_,value,JobType.OccupyJob,_),OccupyJob (vl,zone) ) -> (value,true)
            | _ -> (0,false)
        else
            (0,false)
