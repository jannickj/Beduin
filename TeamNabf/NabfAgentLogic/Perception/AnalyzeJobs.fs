namespace NabfAgentLogic.Perception
module AnalyzeJobs=
    open NabfAgentLogic
    open NabfAgentLogic.AgentTypes
    open NabfAgentLogic.GeneralLib
    open NabfAgentLogic.Logging
    open PerceptionLib

    let onCreatedJob job (state:State) = 
        match job with
        | (_,RepairJob(vn,agent)) when agent <> state.Self.Name-> 
            let updatedFriendlyList =  
                match updateAgentPosition agent vn state.Self.Team false state.FriendlyData with
                | agent::rest when not agent.IsInVisionRange -> {agent with Status = Disabled }::rest
                | _ ->  state.FriendlyData

            { state with  FriendlyData = updatedFriendlyList }

        | _ -> state
    
    let onRemovedJob job (state:State) = 
        match job with
        | (_,RepairJob(_,agent)) when agent <> state.Self.Name -> 
            let updatedFriendlyList =  
                match tryPartionAgentsByName agent state.FriendlyData with
                | Some(agent,rest) when not agent.IsInVisionRange -> {agent with Status = Normal}::rest
                | _ ->  state.FriendlyData

            { state with  FriendlyData = updatedFriendlyList }
        | (_,RepairJob(_,agent)) when agent = state.Self.Name ->
            { state with Relations = Map.remove MyRepairer state.Relations }
        | _ -> state

    let updateStateWithJob job (state:State) =
        match job with 
        | AddedOrChangedJob (((Some id,_,_,_), jobData)) & AddedOrChangedJob job-> 
            
            let jobExists = List.exists (getJobId >> ((=) id)) state.Jobs        
            
            if jobExists then
                let otherJobs = List.filter (getJobId >> ((<>) id)) state.Jobs
                { state with Jobs =  job::otherJobs }
            else
                { state with Jobs = job::state.Jobs }
                |> onCreatedJob job

        | RemovedJob ((Some id,_,_,_), jobData) & RemovedJob job -> 
            let newJobs = List.filter (getJobId >> ((<>) id)) state.Jobs
            let newMyJobs = List.filter (fst >> ((<>) id)) state.MyJobs
            { state with Jobs =  newJobs; MyJobs = newMyJobs }
            |> onRemovedJob job

        | AcceptedJob (jobID, vertexName) ->
            logStateInfo state Perception <| sprintf "Added job w. id %A to myjobs. Round is %A" jobID state.SimulationStep
            if not <| List.exists ((=) (jobID,vertexName)) state.MyJobs 
            then
                { state with MyJobs = (jobID, vertexName)::state.MyJobs }
            else
                state

        | FiredFrom id -> 
            logStateInfo state Perception <| sprintf "received fired from job with id %A" id
            let newMyJobs = List.filter (fst >> ((<>) id)) state.MyJobs

            { state with MyJobs =  newMyJobs }    
        | AddedOrChangedJob (((None,_,_,_), _))
        | RemovedJob ((None,_,_,_), _) ->
            logStateError state Perception <| sprintf "No Job id given for job"
            state
        | AddedOrChangedJob (((Some _,_,_,_), _)) -> state //bug in f# cant detect the other match
        | RemovedJob (((Some _,_,_,_), _)) -> state //bug in f# cant detect the other match