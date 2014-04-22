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

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let findNewZone (s:State) = None

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