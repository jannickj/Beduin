namespace NabfAgentLogic
module LogicLib =
    
    open FsPlanning.Agent.Planning
    open AgentTypes
    open Graphing.Graph
    open Constants
    open FsPlanning.Search
    

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
        
    let checkIfEnemyOnNode state node =
        let agentlist = List.filter (fun a -> a.Node = node) state.EnemyData
        agentlist.Length >= 1
        
    let nearbyAllies state = 
        List.filter (fun a -> nodeListContains a.Node (neighbourNodes state state.Self)) state.FriendlyData 

    let getJobsByType (jobtype:JobType) (list : Job list) : Job list = List.filter 
                                                                        (
                                                                            fun j -> 
                                                                                match j with
                                                                                | ((_, _, jt, _), _) when jt = jobtype -> true
                                                                                | _ -> false
                                                                        ) list

    let getJobId (job:Job) =
        let ((id, _, _, _),_) = job
        id

    let getJobFromJobID (s:State) (jid:JobID) : Job =
        (List.filter (fun j -> (getJobId j).Value = jid) s.Jobs).Head

    let excludeLesserJobs (s:State) calculateDesire (jobs:Job list) =
        if (s.MyJobs.IsEmpty) then
            jobs
        else
            let (id,_) = s.MyJobs.Head
            List.filter (fun j -> (calculateDesire j s) > (calculateDesire (getJobFromJobID s id) s)) jobs


    let createApplication id desire = 
        Communicate(ApplyJob(id,desire))     
        
    let createApplicationList state jobtype calculateDesire = 
        List.map (
                    fun (job:Job) -> 
                        let id = (getJobId job).Value
                        let desire = (calculateDesire job state)
                        (createApplication id desire)
                 ) 
                 (excludeLesserJobs state calculateDesire (getJobsByType jobtype state.Jobs))

    let getJobValueFromJoblist (list:(JobID*_) list) (s:State) : int =
        let (id,_) = list.Head
        let ((_,value,_,_),_) = (getJobFromJobID s id)
        value

    //pathfind through the graph. When the path is found, count it's length and register each node which is held by an enemy
    //returns: (dist to job * number of enemy node)
    let getDistanceToJobAndNumberOfEnemyNodes (targetNode:VertexName) (s:State) =
        let number_of_enemy_nodes = 0.0
        let distance_to_job = 1.0
        

        (distance_to_job   ,   1.0  -  (number_of_enemy_nodes  *  DESIRE_COST_OF_MOVING_THROUGH_ONE_ENEMY_NODE)) 

    


    //let isPartOfOccupyJob n (s:State) = List.exists (fun (j:Job) -> j ) s.Jobs


    let distanceBetweenNodes node1 node2 (state:State) : int =
                if state.HeuristicMap.ContainsKey(node1, node2) then 
                    state.HeuristicMap.[node1, node2]
                else
                    666
    let distanceBetweenAgentAndNode node state : int = distanceBetweenNodes state.Self.Node node state
    
    let findTargetNode startNode condition (state:State) = 
        let nodesWithCond = List.filter (condition state) <| (List.map fst <| Map.toList state.World)
        let distNodes = List.map (fun v -> ((distanceBetweenNodes startNode v state),v)) nodesWithCond        
        match distNodes with
        | [] -> None
        | [single] -> Some <| snd single
        | nodes -> Some ( snd <| List.min nodes )

        
    let findAndDo startNode condition actionString (inputState:State) =
        let targetOpt = findTargetNode startNode condition inputState
        match targetOpt with
        | None -> None
        | Some target ->
                Some
                        (   "go to node " + target + " and " + actionString
                        ,   Activity
                        ,   [
                                Requirement <| ((fun state -> (state.Self.Node = target)), Some (distanceBetweenAgentAndNode target))
                            ;   Requirement(
                                            (fun state -> not <| condition state target
                                                            
                                            ), None
                                           )
                            ]
                        )