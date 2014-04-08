namespace NabfAgentLogic

module SaboteurLogic =

    open AgentTypes
    open AgentLogicLib
    open PathFinding
    open Constants

    let saboteurRank (allies:Agent list) (self:Agent) =
        let order = List.sort (self :: allies)
        (List.findIndex (fun a -> a = self) order) + 1

    let rec getTarget (l : Agent list list) (rank : int) =
        match l with
        | head :: tail -> if head.Length >= rank then Some head.[rank-1] else getTarget tail (rank-head.Length)
        | [] -> None

    let saboteurReact (s:State) (agents:Agent list * Agent list) =
        let allySabs = List.filter (fun a -> a.Role = Some Saboteur) (snd agents)
        let myRank = saboteurRank allySabs s.Self

        let enemySabs = List.filter (fun a -> a.Role = Some Saboteur) (fst agents)
        let repairers = List.filter (fun a -> a.Role = Some Repairer) (fst agents)
        let unknowns = List.filter (fun a -> a.Role = None) (fst agents)
        let explorers = List.filter (fun a -> a.Role = Some Explorer) (fst agents)
        let inspectors = List.filter (fun a -> a.Role = Some Inspector) (fst agents)
        let sentinels = List.filter (fun a -> a.Role = Some Sentinel) (fst agents)

        let target = getTarget [enemySabs;repairers;unknowns;explorers;inspectors;sentinels] myRank
        match target with
        | Some t -> tryDo (Attack(t.Name)) s
        | None -> (false, None)

    let rec findAttackGoal (g:Goal list) =
        match g with
        | head :: tail -> 
            match head with
            | JobGoal(AttackGoal(v)) -> Some v
            | _ -> findAttackGoal tail
        | [] -> None

    let workOnAttackGoal (s:State) =
        match (findAttackGoal s.Goals) with
        | Some v ->
            let goal = pathTo s.Self v s.World
            match goal with
            | Some vl -> tryGo s.World.[vl.Head] s
            | None -> (false,None)
        | None -> (false,None)

   /////////////////////////////////////
   ///  DECIDE JOBS
   /////////////////////////////////////

    let decideJobSaboteur (s:State) (job:Job) =  
        match job with
        | ((_,_,JobType.AttackJob,_),AttackJob (vl) ) -> desireFromPath s.Self s.World vl.Head SABOTEUR_ATTACKJOB_MOD
        | ((_,_,JobType.OccupyJob,_),OccupyJob (vl,zone) ) -> desireFromPath s.Self s.World vl.Head SABOTEUR_OCCUPYJOB_MOD
        | ((_,_,JobType.DisruptJob,_),DisruptJob (vn)) -> desireFromPath s.Self s.World vn SABOTEUR_DISRUPTJOB_MOD
        | _ -> (0,false)