namespace NabfAgentLogic
module Explorer =

    
    open AgentTypes
    open Graphing.Graph

    ///////////////////////////////////Helper functions//////////////////////////////////////
        
    //Check if any jobs contain the current vertex.
    let checkZoneCandidate (s:State) =
        let occupyJobs = List.filter (fun (((_,_,jobtype,_),data):Job) -> jobtype = JobType.OccupyJob) s.Jobs
        let l = List.filter (fun ((_,OccupyJob(_,vertices)):Job) -> (List.exists (fun (vn:VertexName) -> s.Self.Node = vn ) vertices)) occupyJobs
        l <> []

    ////////////////////////////////////////Logic////////////////////////////////////////////

    