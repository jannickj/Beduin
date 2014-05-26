namespace NabfAgentLogic
module Common =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants

    


    ///////////////////////////////////Helper functions//////////////////////////////////////
    
    //Calculate the desire to an occupy job
    let calculateDesireOccupyJob  modifier (j:Job) (s:State)= 
        let ((_,newValue,_,_),(jobData)) = j      
        let oldJobValue = 
                            if (s.MyJobs.IsEmpty) then
                                0
                            else
                                (getJobValueFromJoblist s.MyJobs s)

        let jobTargetNode = 
            match jobData with
            | OccupyJob (_,zone) -> zone.Head
        

        let (distanceToJob,personalValueMod) = (getDistanceToJobAndNumberOfEnemyNodes jobTargetNode s)
        
        //final desire
        int <| (((float newValue) * personalValueMod) - (float oldJobValue))    +     (-(distanceToJob * DISTANCE_TO_OCCUPY_JOB_MOD))    +    modifier


    //Try to find any repair jobs put up by the agent itself.
    let rec tryFindRepairJob (s:State) (knownJobs:Job list) =
            match knownJobs with
            | (_ , rdata) :: tail -> if rdata = RepairJob(s.Self.Node,s.Self.Name) then Some knownJobs.Head else tryFindRepairJob s tail
            | [] -> None

    ////////////////////////////////////////Logic////////////////////////////////////////////

    //An agent always wants to have exactly one goal
    let onlyOneJob s = None// Some("have exactly 1 job.",Inherent,[Requirement(fun state -> state.Jobs.Length = 1)])

    //Try to make it so the agent has explored one more node
    let exploreMap (s:State) = 
        if s.MyExploredCount < s.TotalNodeCount
        then
            let count = s.MyExploredCount
            Some("explore one more node.",Activity,[Requirement(fun state -> state.MyExploredCount > count)])
        else
            None

    //When disabled, post a repair job, then recharge while waiting for a repairer. Temporary version to be updated later.
    //Works by creating a plan to recharge one turn each turn.
    let getRepaired (s:State) = 
        if s.Self.Status = Disabled 
        then
            let j = tryFindRepairJob s s.Jobs
            let myName = s.Self.Name
            match j with
            //I already created a job:
            | Some(_,RepairJob(_,myName)) -> 
                Some("wait for a repairer.",Activity,[Plan(fun s -> [Perform(Recharge)])])
            //Otherwise, create the job, then start waiting
            | _ -> 
                let here = s.Self.Node
                Some("get repaired.",Activity,[Plan(fun state -> [
                                                                 Communicate( CreateJob( (None,5,JobType.RepairJob,1),RepairJob(s.Self.Node,s.Self.Name) ) )
                                                                 ]);Requirement(fun state -> state.LastAction = Recharge)])
        else
            None

    //Find a node of at leas value 8 to stand on.
    let generateMinimumValue (s:State) = Some("find a good node to occupy.",Activity,[Requirement(fun state -> state.World.[state.Self.Node].Value.IsSome && state.World.[state.Self.Node].Value.Value >= MINIMUM_VALUE_VALUE)])

    let shareKnowledge (s:State) : Option<Intention> =
         Some ("share my knowledge", Communication, [Plan (fun s -> [(Communicate <| ShareKnowledge ( s.NewKnowledge))] )])
    
    
    let applyToOccupyJob  modifier (s:State) = 
        let applicationList = createApplicationList s JobType.OccupyJob (calculateDesireOccupyJob modifier)
        Some(
                "apply to all occupy jobs"
                , Communication
                , [Plan(fun state -> applicationList)]
            )
    

    let workOnOccupyJob (s:State) =
        let myJobs = List.map (fun (id,_) -> getJobFromJobID s id) s.MyJobs
        let myOccupyJobs = getJobsByType JobType.OccupyJob myJobs
        match myOccupyJobs with
        | ((id,_,_,_),_)::_ -> 
            let (_,node) = List.find (fun (jid,_) -> id.Value = jid) s.MyJobs
            Some
                (   "occupy node " + node
                ,   Activity
                ,   [
                        Requirement <| fun state -> state.Self.Node = node
                    ;   Plan <| fun _ -> [Perform Recharge]
                    ]
                )
        | [] -> None