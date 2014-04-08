namespace NabfAgentLogic

module SentinelLogic =

    open AgentTypes
    open AgentLogicLib
    open PathFinding
    open Constants

    let sentinelReact (s:State) (agents:Agent list * Agent list) =
        let potentialEnemySabs = List.filter (fun a -> (a.Role = Some Saboteur || a.Role = None)) (fst agents)
        if not potentialEnemySabs.IsEmpty then tryDo Parry s else (false,None)

//    let rec findOccupyGoal (g:Goal list) =
//        match g with
//        | head :: tail -> 
//            match head with
//            | JobGoal(OccupyGoal(v)) -> Some v
//            | _ -> findOccupyGoal tail
//        | [] -> None
//
//    let workOnOccupyGoal (s:State) =
//        match (findOccupyGoal s.Goals) with
//        | Some v ->
//            let goal = pathTo s.Self v s.World
//            match goal with
//            | Some vl -> tryGo s.World.[vl.Head] s
//            | None -> (false,None)
//        | None -> (false,None)

   /////////////////////////////////////
   ///  DECIDE JOBS
   /////////////////////////////////////

    let decideJobSentinel (s:State) (job:Job) =  
        match job with
        | ((_,_,JobType.OccupyJob,_),OccupyJob (vl,zone) ) -> desireFromPath s.Self s.World vl.Head SENTINEL_OCCUPYJOB_MOD
        | ((_,_,JobType.DisruptJob,_),DisruptJob (vn)) -> desireFromPath s.Self s.World vn SENTINEL_DISRUPTJOB_MOD
        | _ -> (0,false)