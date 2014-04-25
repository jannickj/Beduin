namespace NabfAgentLogic
module Explorer =

    
    open AgentTypes
    open Graphing.Graph
    open Constants

    ///////////////////////////////////Helper functions//////////////////////////////////////
        
    //Check if any jobs contain the current vertex.
    let checkZoneCandidate (s:State) =
        let occupyJobs = List.filter (fun (((_,_,jobtype,_),data):Job) -> jobtype = JobType.OccupyJob) s.Jobs
        let l = List.filter (fun ((_,OccupyJob(_,vertices)):Job) -> (List.exists (fun (vn:VertexName) -> s.Self.Node = vn ) vertices)) occupyJobs
        l <> []

    let hasExploredPhase1 (s:State) = (float s.ExploredCount) > ( EXPLORE_FACTOR_LIGHT * (float s.TotalNodeCount) )

    let onHighValueNode (s:State) = s.World.[s.Self.Node].Value.Value >= ZONE_ORIGIN_VALUE

    let nodePartOfZone (s:State) =
        let occupyJobs = (List.filter (fun ((_,_,jType,_),_) -> jType = JobType.OccupyJob) s.Jobs)
        let occupyJobSet = Set.ofList (List.concat (List.map (fun (_,OccupyJob(l,_)) -> l) occupyJobs))
        Set.contains s.Self.Node occupyJobSet

    let nodeHostile (s:State) = true // Not implemented yet!

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


    //Recursively check if a zone has been explored
    let rec isDoneExploring node explored (s:State) =
        let neighbourSet = Set.map (fun (_,st) -> st) s.World.[node].Edges
        let newExplored = Set.add s.Self.Node explored
        //Returns: (is not valuable) or (is valuable && has no neighbours that are neither done or in the explored set)
        (not (hasValueHigherThan node ZONE_BORDER_VALUE s)) || (hasValueHigherThan node ZONE_BORDER_VALUE s && not (Set.exists (fun n -> not (Set.contains n explored) && not (isDoneExploring n newExplored s)) neighbourSet))
        

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let findNewZone (s:State) = 
        if (newZoneFound s)
        then
            let origin = s.Self.Node
            Some("probe a new zone.",Activity,[
                                               Requirement(fun state -> isDoneExploring origin Set.empty state); 
                                               Plan(fun state -> []
                                                    //let x = 1
                                                    //[Communicate( CreateJob( (None,5,JobType.OccupyJob,1),RepairJob(s.Self.Node,s.Self.Name) ) )]
                                               )])
        else
            None

    let applyToOccupyJob (s:State) = None

    let workOnOccupyJob (s:State) = None

    let findNodeToProbeUnconditional (s:State) = 
        if s.ProbedCount < s.TotalNodeCount
            then
                let myOldProbedCount = s.MyProbedCount
                Some("probe one more node.",Activity,[Requirement(fun state -> state.MyProbedCount > myOldProbedCount)])
            else
                None

    let findNodeToProbePhase1 (s:State) = if(hasExploredPhase1 s) then findNodeToProbeUnconditional s else None