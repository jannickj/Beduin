namespace NabfAgentLogic
module Common =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants
    open Graphing.Graph
    open Logging
    open GeneralLib

    ///////////////////////////////////Helper functions//////////////////////////////////////

    //Calculate value of repairing of an agenttype
    let calculateRepairValue agentType =
        match agentType with
        | Saboteur -> SABOTEUR_REPAIR_PRIORITY
        | Repairer -> REPAIRER_REPAIR_PRIORITY 
        | Sentinel -> SENTINEL_REPAIR_PRIORITY
        | Explorer -> EXPLORER_REPAIR_PRIORITY
        | Inspector -> INSPECTOR_REPAIR_PRIORITY
       

    //Calculate the desire to an occupy job
    let calculateJobDesire importanceModifier distanceModifier (roleModifier:float) (j:Job) (s:State)= 
        let ((_,jobValue,_,_),(jobData)) = j      
        let targetNode = 
            match jobData with
            | OccupyJob (_,n::_) -> n
            | RepairJob (n,_) -> n
            | DisruptJob (n) -> n
            | AttackJob (n::_,_) -> n
            | _ -> failwith <| sprintf "Can't calculate distance desire as %A has no target node" j

        let distanceToJob = (distanceBetweenAgentAndNode targetNode s)

        let currentJobValue = 
            match myBestCurrentJob s with
            | Some bestJob -> getJobValue bestJob
            | None -> 0
        
        let importance = importanceModifier * (float (jobValue - currentJobValue))

        let distanceAndRole = distanceModifier * (float (distanceToJob - (int roleModifier)))
        //final desire
        int <| importance - distanceAndRole


    //Try to find any repair jobs for a specific agent
    let tryFindRepairJobForAgent (agentName) (jobs:Job list) =
        List.tryFind (fun (_,job) -> 
                            match job with
                            | RepairJob (_,a) -> agentName = a
                            | _ -> false) jobs



    let nodeHasMinValue (state:State) node =
        let n = state.World.[node] 
        if (n.Value.IsSome) then
            n.Value.Value >= MINIMUM_VALUE_VALUE
        else
            false
            
    let exploringNotDone (s:State) = (float s.World.Count) < ( EXPLORE_FACTOR_DONE_EXPLORING * (float s.TotalNodeCount) )

    let notSurveyedEnough (s:State) = s.SimulationStep < SURVEY_MY_NODE_UNTIL_THIS_TURN_IF_NEEDED

    ////////////////////////////////////////Immediate Actions/////////////////////////////////
    let immediateAction state =
        match state.Self.Role with
        | Some Saboteur when state.Self.Status <> EntityStatus.Disabled ->
            let relevantEnemies = List.filter shouldAttack <| enemiesHere state state.Self.Node
            let roleListMap = Map.ofSeq <| Seq.groupBy (fun agent -> agent.Role) relevantEnemies
            let roleList role = if Map.containsKey role roleListMap then List.ofSeq roleListMap.[role]
                                else []

            let friendlySabsHere = 
                List.length (List.filter (fun agent -> agent.Role = Some Saboteur) <| alliesHere state state.Self.Node)
            
            // We prioritize repairers that we can destroy this turn (destroying a repairer in a single turn takes two attacks)
            let prioritizedRepairers = List.ofSeq <| Seq.take (friendlySabsHere / 2) (roleList (Some Repairer))
            let restRepairers = List.ofSeq <| Seq.skip (friendlySabsHere / 2) (roleList (Some Repairer))

            let priorityList = 
                  prioritizedRepairers @ prioritizedRepairers // insert this twice so that each prioritized repairer is attacked by two agents
                                                              // Could be done prettier / clearer?
                @ roleList (Some Saboteur)
                @ restRepairers
                @ roleList None // Unknown agents could be saboteurs or repairers
                @ roleList (Some Sentinel)
                @ roleList (Some Explorer)
                @ roleList (Some Inspector)
               
            match selectBasedOnRank state priorityList with
            | Some agent -> Some <| Perform (Attack agent.Name)
            | None -> None

        | _ -> None


    ////////////////////////////////////////Logic////////////////////////////////////////////




    let onlyHaveOneJob (inputState:State) =
        match myBestCurrentJob inputState with
        | Some myBestJob -> 
            match List.filter (fun (id,_) -> getJobId myBestJob <> id) inputState.MyJobs with 
            | [] -> None
            | otherJobs -> 
                let unapplyOtherJobs = List.map (fun (id,_) -> Communicate <| UnapplyJob(id)) otherJobs
                Some <| normalIntention ("only have one job", Communication,[Plan (fun _ -> Some <| unapplyOtherJobs)] )
        | None -> None

    let unapplyFromJobsWhenDisabled (inputState:State) = 
        if inputState.MyJobs.Length = 0 || inputState.Self.Status = EntityStatus.Normal then
            None
        else
            let (jobid,_) = inputState.MyJobs.Head
            Some <| normalIntention 
                        ( "unapply from my job with id " + jobid.ToString() + " because im disabled"
                            , Communication
                            , [ Plan <| fun state -> Some [Communicate( UnapplyJob(jobid) )]]
                            )


    //Try to make it so the agent has explored one more node
    let exploreMap (inputState:State) = 
        let othersOnMyNode = List.filter (fun a -> a.Node = inputState.Self.Node && not(a.Name = inputState.Self.Name)) inputState.FriendlyData
        let otherAgentNames = List.map getAgentName othersOnMyNode
        let tryNearestUnexplored = nearestVertexSatisfying inputState isUnexplored
        match tryNearestUnexplored with
        | Some nearestUnexplored ->
            let goal = 
                if (nodeHasNoOtherFriendlyAgentsOnIt inputState inputState.Self.Node) then
                    Explored nearestUnexplored
                else
                    if (myRankIsGreatest inputState.Self.Name otherAgentNames) then
                        let nextBest = findNextBestUnexplored inputState
                        match nextBest with
                        | Some vertex -> Explored vertex
                        | None -> Explored nearestUnexplored
                    else
                        Explored nearestUnexplored

            Some <| normalIntention 
                 ( "explore one more node."
                 , Activity
                 , [Requirement goal]
                 )
        | _ -> None

    //Posts or removes a repair job on the master server for repairing itself
    let postRepairJob (inputState:State) =
        match tryFindRepairJobForAgent inputState.Self.Name inputState.Jobs, inputState.Self.Status with
            | None, Disabled ->
                let myNode = inputState.Self.Node
                let myName = inputState.Self.Name
                let jobPriority = calculateRepairValue inputState.Self.Role.Value
                let numberOfAgentsNeeded = 1
                let communicateJob = CreateJob ( (None,jobPriority,JobType.RepairJob,numberOfAgentsNeeded),RepairJob(myNode,myName))
                normalIntention 
                    ( "post a repair job for me."
                    , Communication
                    , [ Plan <| fun _ -> Some [ Communicate <| communicateJob ]]
                    )
                |> Some
            | Some ((Some id,_,_,_),_),Normal ->
                normalIntention 
                    ( "remove the repair job for me."
                    , Communication
                    , [ Plan <| fun _ -> Some [ Communicate <| RemoveJob id ]]
                    )
                |> Some
            | _ -> None    

    //When disabled, tries to locate repairer if None found continue with normal duty
    let getRepaired (inputState:State) =
        let repairJob =
            match inputState.MyJobs with
            | (id,_)::_ -> Some (snd <| getJobFromJobID inputState.Jobs id)
            | _ -> None

        match Map.tryFind MyRepairer inputState.Relations, inputState.Self.Status, repairJob with
        | Some aName, Disabled, Some (RepairJob(_,rName)) when aName = rName && myRankIsGreatest inputState.Self.Name [rName] -> 
            None
        | Some aName,Disabled,_ ->
            normalIntention 
                (   "get repaired by "+aName,
                    Activity,
                    [   Requirement (GetCloseTo aName)
                    ;   Plan (fun s -> if s.Self.Status = Disabled then Some [Perform(Recharge)] else None)
                    ]
                )
            |> Some
        | _ -> None

    let giveMyLocationToMyRepairer (inputState:State) =
        match (inputState.Self.Status, Map.tryFind MyRepairer inputState.Relations) with
        | (Disabled, Some repairer) -> 
            normalIntention 
                ( "send my location to repairer "+repairer,
                  Communication,
                  [
                    Plan (fun s -> Some [Communicate <| SendMail (s.Self.Name,repairer,MyLocation s.Self.Node)])
                  ]
                )
            |> Some
        | _ -> None 

            
    //Find a node of at least value 8 to stand on.
    let generateSomeValue (inputState:State) = 
        Some  <| normalIntention ( "get some(8) value"
             , Activity
             , [ Requirement <| AtMinValueNodeNotPartOfZone SOME_VALUE_VALUE
               ; Plan <| fun _ -> Some ([Perform Recharge]) 
               ]
             )

    //Find a node of at least value 6 to stand on.
    let generateLittleValue (inputState:State) = 
        Some  <| normalIntention ( "get little(6) value"
             , Activity
             , [ Requirement <| AtMinValueNodeNotPartOfZone LITTLE_VALUE_VALUE
               ; Plan <| fun _ -> Some ([Perform Recharge]) 
               ]
             )

    //Find a node of at least value 4 to stand on.
    let generateLeastValue (inputState:State) = 
        Some  <| normalIntention ( "get least(4) value"
             , Activity
             , [ Requirement <| AtMinValueNodeNotPartOfZone LEAST_VALUE_VALUE
               ; Plan <| fun _ -> Some ([Perform Recharge]) 
               ]
             )

    //Find a node of at least value 2 to stand on.
    let generateMinimumValue (inputState:State) = 
        Some <| normalIntention  ( "get minimum(2) value"
             , Activity
             , [ Requirement <| AtMinValueNodeNotPartOfZone MINIMUM_VALUE_VALUE
               ; Plan <| fun _ -> Some ([Perform Recharge]) 
               ]
             )


    let shareKnowledge (inputState:State) : Option<Intention> =
        logInfo Intentions <| sprintf "length of newKnowledge: %A" inputState.NewKnowledge.Length 
        let hasNewKnowledge s = List.length s.NewKnowledge > 0
        if hasNewKnowledge inputState then
            Some<| normalIntention 
                    (   "share my knowledge", 
                        Communication, 
                        [Plan   ( fun state -> 
                                    if hasNewKnowledge state then
                                        Some [(Communicate <| ShareKnowledge ( state.NewKnowledge))] 
                                    else
                                        None
                                )
                        ]
                    )
        else None
         
    
    
    let applyToOccupyJob  modifier (inputState:State) = 
        logStateImportant inputState Jobs <| sprintf "Agent has %A jobs" inputState.MyJobs.Length
        if inputState.Self.Status = Normal then
            let desireCalc = (calculateJobDesire JOB_IMPORTANCE_MODIFIER_OCCUPY DISTANCE_TO_OCCUPY_JOB_MOD  modifier)
            let applicationList = createApplicationList inputState JobType.OccupyJob desireCalc
            let numberofApps = List.length applicationList
            if numberofApps > 0 then
                Some <| normalIntention ( 
                        "apply to " + numberofApps.ToString() + " occupy jobs"
                            , Communication
                            , [Plan (fun state -> Some applicationList)]
                        )
            else
                None
        else 
            None
    

    let workOnOccupyJob (inputState:State) =
        logStateInfo inputState Intentions <| sprintf  "my jobs are: %A" (List.map fst inputState.MyJobs)
        let myJobs = List.map (fun (id,_) -> getJobFromJobID inputState.Jobs id) inputState.MyJobs
        let myOccupyJobs = getJobsByType JobType.OccupyJob myJobs
        match myOccupyJobs with
        | ((id,_,_,_),_)::_ -> 
            let (_,node) = List.find (fun (jid,_) -> id.Value = jid) inputState.MyJobs
            Some <| normalIntention 
                ( "occupy node " + node
                 , Activity
                 , [ Requirement (At node)
                   ; Plan <| fun _ -> Some [Perform Recharge]
                   ]
                 )
        | [] -> None


    let postAttackJob (inputState:State) = 
        if inputState.SimulationStep % 5 <> 0 then
            None
        else
            let agentsICanSee = List.filter (fun a -> a.Node <> "" && a.Status = EntityStatus.Normal) inputState.EnemyData
            let agentsOnValuableNode = List.filter 
                                                (fun a -> 
                                                    inputState.World.[a.Node].Value.IsSome
                                                    && 
                                                    inputState.World.[a.Node].Value >= Some MIN_NODE_VALUE_TO_POST_ATTACK
                                                )             
                                                agentsICanSee

            let valuableEnemyControlledNodes = Set.filter (fun n -> 
                                                                inputState.World.[n].Value.IsSome
                                                                && 
                                                                inputState.World.[n].Value >= Some MIN_NODE_VALUE_TO_POST_ATTACK
                                                           ) 
                                                           inputState.NodesControlledByEnemy

            let agentTargetExists = agentsOnValuableNode.Length > 0 
            let nodeTargetExists = valuableEnemyControlledNodes.Count > 0
            if (agentTargetExists || nodeTargetExists) then            
                let node = 
                    if (agentTargetExists) then
                        (List.maxBy (fun a -> inputState.World.[a.Node].Value) agentsOnValuableNode).Node
                    else
                        List.maxBy (fun vertexName -> inputState.World.[vertexName].Value) (Set.toList valuableEnemyControlledNodes)

                let isAttackJobOnNode onNode job =
                    match job with
                    | (_,AttackJob(attackNode::_,_)) -> attackNode = onNode
                    | _ -> false
                
                
                let jobValue = inputState.World.[node].Value.Value * ATTACK_IMPORTANCE_MODIFIER
                match List.tryFind (isAttackJobOnNode node) inputState.Jobs with
                | Some ((id,_,_,_),_) ->
                    Some <| normalIntention 
                            ( "update attack job on node " + node
                            , Communication
                            , [ Plan <| fun state -> Some [Communicate( UpdateJob((id,jobValue,JobType.AttackJob,1),AttackJob([node],inputState.SimulationStep) ))]]
                            )
                | None ->
                    Some <| normalIntention 
                            ( "post attack job on node " + node
                             , Communication
                             , [ Plan <| fun state -> Some [Communicate( CreateJob( (None,jobValue,JobType.AttackJob,1),AttackJob([node],inputState.SimulationStep) ))]]
                             )
            else
                None


    let postDefenseJob (inputState:State) = 
        if inputState.SimulationStep % 5 <> 0 then
            None
        else
            let shouldReportForZone zone agent = List.exists ((=) agent.Node) zone && shouldAttack agent 
            //checking if the agent has an occupy job and that an non-disabled enemy is standing on it's node
            let agentsInMyZoneWhileImOnOccupyJob =
                match inputState.MyJobs with
                | (jobid,_)::_ -> 
                    match (getJobFromJobID inputState.Jobs jobid) with
                    | (_,OccupyJob(_,zoneList)) -> 
                        match List.filter (shouldReportForZone zoneList) inputState.EnemyData with
                        | [] -> None
                        | reportAgents -> Some reportAgents
                    | _ -> None
                | _ -> None
            
            if (agentsInMyZoneWhileImOnOccupyJob.IsNone) then
                None
            else 
                match inputState.MyJobs.Head with
                | (_,vertex) when vertex <> inputState.Self.Node-> None //if we have not arrived at the job yet, dont post defense
                | (id,_) -> 
                    let ((_,occupyValue,_,_),_) = getJobFromJobID inputState.Jobs id
                    let jobValue = occupyValue * DEFENSE_IMPORTANCE_MODIFIER
                    let isAttackJobOnNode onNode job =
                        match job with
                        | (_,AttackJob(aNodes,_)) -> List.exists ((=) onNode) aNodes
                        | _ -> false
                        
                    let jobNodes = 
                        match (getJobFromJobID inputState.Jobs (fst inputState.MyJobs.Head)) with
                        | (_,OccupyJob(_,ns)) ->  ns
                        | _ -> failwith "Hoer her jannick, det her kan ikke ske. -And"
                    
                    
                    let existingJob job =  List.exists (fun node -> isAttackJobOnNode node job) jobNodes  
                    match List.filter existingJob inputState.Jobs with
                    | [((id,_,_,_),_)] ->
                        Some <| normalIntention 
                                ( sprintf "update defense job on nodes %A" jobNodes
                                , Communication
                                , [ Plan <| fun state -> Some [Communicate( UpdateJob((id,jobValue,JobType.AttackJob,1),AttackJob(jobNodes,inputState.SimulationStep) ))]]
                                )
                    | [] ->
                        Some <| normalIntention 
                                ( sprintf "post defense job on nodes %A" jobNodes
                                , Communication
                                , [ Plan <| fun state -> Some [Communicate( CreateJob( (None,jobValue,JobType.AttackJob,1),AttackJob(jobNodes,inputState.SimulationStep) ))]]
                                )
                    | multipleJobs -> 
                        let updateZoneOfOldJob nodesOfNewJob jobToBeUpdated =
                            match jobToBeUpdated with
                            | ((oldId,oldValue,_,_),AttackJob(nodesOfOld,timeStamp)) -> 
                                let newAttackJobNodes = List.filter (fun n -> List.exists ((<>) n) nodesOfNewJob) nodesOfOld
                                Plan <| fun state -> Some [Communicate( UpdateJob((oldId,oldValue,JobType.AttackJob,1),AttackJob(newAttackJobNodes,timeStamp) ))]
                            | _ -> failwith "job list includes non-attack jobs"

                        let allJobUpdates = List.map (updateZoneOfOldJob jobNodes) multipleJobs
                        
                        Some <| normalIntention 
                                ( sprintf "post defense job on nodes %A" jobNodes
                                , Communication
                                , allJobUpdates@[ Plan <| fun state -> Some [Communicate( CreateJob( (None,jobValue,JobType.AttackJob,1),AttackJob(jobNodes,inputState.SimulationStep) ))]]
                                )

                            


