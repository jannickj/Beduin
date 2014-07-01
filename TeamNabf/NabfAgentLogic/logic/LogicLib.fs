namespace NabfAgentLogic
module LogicLib =
    
    open FsPlanning.Agent.Planning
    open AgentTypes
    open Graphing.Graph
    open Constants
    open FsPlanning.Search
    open FsPlanning.Search.Problem
    open ActionSpecifications
    open GeneralLib

    let flip f x y = f y x

    let normalIntention (label,intentionType,objectives) =
        {
            Label = label;
            Type = intentionType;
            Objectives = objectives;
            ChangeStateAfter = None
            ChangeStateBefore = None
        }
     
    
    let selectBasedOnRank (state:State) things =
        let self = state.Self
        let isSame agent = agent.Node = self.Node && agent.Role = self.Role
        let sameFriendlies = List.filter isSame state.FriendlyData
        let order = List.sortBy getAgentName (self::sameFriendlies)
        let myPos = List.findIndex (getAgentName >> ((=) self.Name)) order
        if List.length things <= myPos then 
            None 
        else
            Some <| List.nth things myPos

    let nodeListContains n (nl:string list) =
        (List.tryFind (fun s -> s = n) nl).IsSome

    let listContains element elementList =
        (List.tryFind (fun s -> s = element) elementList).IsSome

    let probedVertices (world : Graph) =
        List.choose (fun (name,vertex:Vertex)-> if vertex.Value.IsSome then Some name else None ) <| Map.toList world

    //WARNING: DEPRECATED
    let agentHasFulfilledRequirementEnemies aName state func =
        (List.tryFind (fun ag -> (func ag) && ag.Name = aName) state.EnemyData).IsSome
    
    //WARNING: DEPRECATED
    let agentHasFulfilledRequirementFriendlies aName state func =
        (List.tryFind (fun ag -> (func ag) && ag.Name = aName) state.FriendlyData).IsSome
        
    let neighbourNodes state (self:Agent) = 
        List.append (getNeighbourIds self.Node state.World) [self.Node]

    let nearbyEnemies state source = 
        List.filter (fun a -> nodeListContains a.Node (neighbourNodes state source)) state.EnemyData        

    let getPosition (agent:Agent) = agent.Node

    let nearbyAllies state = 
        List.filter (fun a -> nodeListContains a.Node (neighbourNodes state state.Self)) state.FriendlyData 

    let getJobsByType (jobtype:JobType) (list : Job list) : Job list = List.filter 
                                                                        (
                                                                            fun j -> 
                                                                                match j with
                                                                                | ((_, _, jt, _), _) when jt = jobtype -> true
                                                                                | _ -> false
                                                                        ) list
    
    let getJobValue (j:Job) = 
        let ((_,jobValue,_,_),(_)) = j
        jobValue

    let getJobId (job:Job) =
        let ((id, _, _, _),_) = job
        match id with
        | Some jid -> jid
        | None -> failwith <| sprintf "Job: %A, does not have an id" job

    let getJobFromJobID  (jobs:Job list) (jid:JobID) =
        match List.filter (getJobId >> ((=) jid)) jobs with
        | job::_ -> job
        | [] -> failwith <| sprintf "No job with id: %A exists" jid
    
    let getMyJobs (s:State) =
        List.map (fst >> (getJobFromJobID s.Jobs)) s.MyJobs
    
    let agentsInVision agents = 
        let isInVision (agent:Agent) = agent.IsInVisionRange
        List.filter isInVision agents
    
    let agentsAtPos agents pos = 
        List.filter (getPosition >> ((=) pos)) agents

    let getOccupyZone (job:Job) =
        match job with
        | _,OccupyJob(_,zl) -> zl
        | _ -> failwith "Contains non-occupyjob"
    
    let getOccupyAgentPos (job:Job) =
        match job with
        | _,OccupyJob(ap,_) -> ap
        | _ -> failwith "Contains non-occupyjob"

    let shouldAttack (agent:Agent) =
        agent.Status = Normal && 
        (not (agent.Role = Some Sentinel && agent.RoleCertainty >= MINIMUM_ROLE_CERTAINTY))

    let myBestCurrentJob (s:State) = 
        match getMyJobs s with
        | [] -> None
        | myJobs ->
            Some <| List.maxBy getJobValue myJobs

    //Filters a list of jobs to only contain jobs that are better than my current best job
    let selectBestJobs (s:State) (jobs:Job list) =
        match myBestCurrentJob s with
        | Some bestJob ->
            let currentValue = getJobValue bestJob
            let isBetter job = 
                let value = getJobValue job
                value > currentValue
            List.filter isBetter jobs
        | None -> jobs

        //match List.tryFind 
//        if (s.MyJobs.IsEmpty) then
//            jobs
//        else
//            let (id,_) = s.MyJobs.Head
//            List.filter (fun j -> (calculateDesire j s) > (calculateDesire (getJobFromJobID s id) s)) jobs

   
    
    let createApplication calculateDesire state (job:Job) = 
        let id = getJobId job
        let desire = (calculateDesire job state)
        Communicate(ApplyJob(id,desire))

    let createApplicationList state jobtype calculateDesire = 
        let jobsOfProperType = getJobsByType jobtype state.Jobs
        let wantedJobs = selectBestJobs state jobsOfProperType
        let appCreater = createApplication calculateDesire state
        List.map appCreater wantedJobs
                 



    //let isPartOfOccupyJob n (s:State) = List.exists (fun (j:Job) -> j ) s.Jobs

    //Only use this if heuristics for one of the nodes has been calculated
    let edgeDistance node1 node2 (state:State) : int =
        let (heuMap,_) = state.GraphHeuristic
        let [nodeA;nodeB] = List.sort [node1; node2]
        match Map.tryFind (nodeA,nodeB) heuMap with
        | Some (_,dist) ->
            dist
        | None -> failwith "Node %A or Node %A has no graph heuristic calculation and can thus not be used for edgeDistance"

    let distanceBetweenNodes node1 node2 (state:State) : int =
        let (heuMap,_) = state.GraphHeuristic
        let [nodeA;nodeB] = List.sort [node1; node2]
        match Map.tryFind (nodeA,nodeB) heuMap with
        | Some (cost,dist) ->
            //let rechargesRequiredCost = (cost / (state.Self.MaxEnergy.Value/2)) * turnCost state
            let minimumTraversalCost = dist * turnCost state
            minimumTraversalCost + cost
        | None -> INFINITE_HEURISTIC

    let distanceBetweenAgentAndNode node state : int = distanceBetweenNodes state.Self.Node node state  
    
    let findTargetNode startNode condition (state:State) = 
        let nodesWithCond = List.filter (condition state) <| (List.map fst <| Map.toList state.World)
        let distNodes = List.map (fun v -> ((distanceBetweenNodes startNode v state),v)) nodesWithCond        
        match distNodes with
        | [] -> None
        | [single] -> Some <| snd single
        | nodes -> Some ( snd <| List.min nodes )

    let findNextBestNode startNode condition (state:State) = 
        let nodesWithCond = List.filter (condition state) <| (List.map fst <| Map.toList state.World)
        let distNodes = List.map (fun v -> ((distanceBetweenNodes startNode v state),v)) nodesWithCond        
        match distNodes with
        | [] -> None
        | [single] -> Some <| snd single
        | nodes -> 
                    let newNodes = nodes
                    let filteredNodes = List.filter (fun n -> not((List.min nodes) = n) ) newNodes
                    Some ( snd <| List.min filteredNodes )

        
    let findNextBestUnexplored state =
        let isVertexUnExplored vertex = 
            List.forall (fun (cost, _) -> Option.isSome cost) (Set.toList state.World.[vertex].Edges)

        let definiteCost cost = 
            match cost with 
            | Some c -> c
            | None -> Constants.MINIMUM_EDGE_COST

        let goalTest statePair = 
            match statePair with
            | (Some oldVertex, newVertex) when oldVertex <> newVertex -> 
                isVertexUnExplored newVertex
            | _ -> false

        let result (oldVertex, _) (_, resultVertex) =
            match oldVertex with
            | Some vertex -> (Some vertex, resultVertex)
            | None ->
                if isVertexUnExplored resultVertex then
                    (Some resultVertex, resultVertex)
                else
                    (None, resultVertex)

        let pathProblem = 
            { InitialState = (None, state.World.[state.Self.Node].Identifier)
            ; GoalTest = goalTest
            ; Actions = fun (_, vertex) -> Set.toList state.World.[vertex].Edges
            ; Result = result
            ; StepCost = fun _ (cost, _) -> definiteCost cost
            ; Heuristic = fun _ cost -> cost
            }
        
        let solution = Astar.solve Astar.aStar pathProblem (fun () -> false)
        match solution with
        | Some solution -> 
            Some <| snd (List.head <| List.rev solution.Path)
        | None -> None

    let findNextBestUnprobed state =
        let isVertexUnprobed vertex = state.World.ContainsKey(vertex) && state.World.[vertex].Value.IsNone

        let definiteCost cost = 
            match cost with 
            | Some c -> c
            | None -> Constants.MINIMUM_EDGE_COST

        let goalTest statePair = 
            match statePair with
            | (Some oldVertex, newVertex) when oldVertex <> newVertex -> 
                isVertexUnprobed newVertex
            | _ -> false

        let result (oldVertex, _) (_, resultVertex) =
            match oldVertex with
            | Some vertex -> (Some vertex, resultVertex)
            | None ->
                if isVertexUnprobed resultVertex then
                    (Some resultVertex, resultVertex)
                else
                    (None, resultVertex)

        let pathProblem = 
            { InitialState = (None, state.World.[state.Self.Node].Identifier)
            ; GoalTest = goalTest
            ; Actions = fun (_, vertex) -> Set.toList state.World.[vertex].Edges
            ; Result = result
            ; StepCost = fun _ (cost, _) -> definiteCost cost
            ; Heuristic = fun _ cost -> cost
            }
        
        let solution = Astar.solve Astar.aStar pathProblem (fun () -> false)
        match solution with
        | Some solution -> 
            Some <| snd (List.head <| List.rev solution.Path)
        | None -> None

    let myRankIsGreatest myName (other:AgentName List) =
        let qq = List.filter (fun aName -> aName < myName) other
        qq.IsEmpty


    let nearestVertexSatisfying (state : State) (condition : (State -> VertexName -> bool)) =
        let satisfying = List.map fst (Map.toList state.World)
                         |> List.filter (condition state)
        if List.length satisfying > 0 then
            Some (List.minBy (flip distanceBetweenAgentAndNode <| state) satisfying )
        else
            None

        
    let nodeHasNoOtherFriendlyAgentsOnIt (inputState:State) node : bool =
        let friendliesOnNode = List.filter (fun a -> a.Node = node) inputState.FriendlyData
        if (friendliesOnNode.Length = 1) then //is it me standing on the node?
            friendliesOnNode.Head.Name = inputState.Self.Name
        elif (friendliesOnNode.Length = 0) then //no one is standing on the node
            true
        else //more than 1 is standing on the node, including myself, so don't want
            false

            
    let nodeHasEnemyAgent (state:State) node =
        List.exists (fun a -> a.Node = node && a.Status = EntityStatus.Normal) state.EnemyData
