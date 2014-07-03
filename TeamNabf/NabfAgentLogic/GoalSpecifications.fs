namespace NabfAgentLogic
module GoalSpecifications =

    open AgentTypes
    open Graphing.Graph
    open LogicLib
    open Constants
    open Logging
    open GeneralLib
    open ActionSpecifications

    let agentAt agentName state =
        match tryFindAgentByName agentName (state.EnemyData @ state.FriendlyData) with
        | Some agent when agent.Node <> "" ->
            Some agent.Node 
        | _ -> None

    let agentAttacked agent state = 
        let enemy = List.find (fun (enemy : Agent) -> enemy.Name = agent) state.EnemyData
        enemy.Status = Disabled

    let vertexProbed vertex state = 
//        logStateImportant state Planning <| sprintf "vertex probed %A" state.World.[vertex]
        Option.isSome state.World.[vertex].Value

    let vertexInspected vertex state =
        List.forall (fun enemy -> Option.isSome enemy.Role) (enemiesHere state vertex)

    let parried state = 
        state.LastAction = Parry
    
    let charged charge (state : State) =
        match charge with
        | Some charge -> charge >= state.Self.Energy.Value
        | None -> state.LastAction = Recharge

    let agentRepaired agent state =
        match List.tryFind (fun a -> a.Name = agent) state.FriendlyData with
        | Some (agent) -> agent.Status = Normal
        | None -> false
        //state.LastAction = Repair agent
    
    let getCloseTo (agentName:AgentName) (state:State) =
        match tryFindAgentByName agentName state.FriendlyData with
        | Some ({Node = vn}) when vn <> "" && Map.containsKey vn state.World-> 
            let nodes = vn::(getNeighbourIds vn state.World)
            List.exists ((=) state.Self.Node) nodes
        | _ -> 
            failwith ("Cannot reach state get close to "+agentName+" as it does not exist")
            false    

    let atMinValueNodeNotPartOfZone value state = 
        let n = state.World.[state.Self.Node] 
        let occupyJobs = List.filter (fun ((_,_,jobtype,_),_) -> jobtype = JobType.OccupyJob) state.Jobs
        let zonesOfOccupyJob = List.map (fun (_,OccupyJob(_,zoneList))-> zoneList) occupyJobs
        let allNodesInOccupyJobs = List.concat zonesOfOccupyJob
        if (n.Value.IsSome && not (List.exists (fun name -> name = n.Identifier) allNodesInOccupyJobs) && nodeHasNoOtherFriendlyAgentsOnIt state n.Identifier) then
            n.Value.Value >= value
        else
            false
    
    let killAll vertex (state:State) =
        let killable enemies = 
            List.filter (getPosition >> ((=) vertex)) state.EnemyData
            |> List.filter shouldAttack
        
        Set.contains vertex state.NodesInVisionRange 
        && List.length <| killable state.EnemyData = 0

    let surveyedNeighbourEdges state = 
        let rangeOneEdges = state.World.[state.Self.Node].Edges          
        0 = (Set.count <| Set.filter (fun (value,_) -> Option.isNone value) rangeOneEdges)
    

    let distanceHeuristics vertex =
        distanceBetweenAgentAndNode vertex
    
    let agentDistanceHeuristics agent state =
        match agentAt agent state with
        | Some vn -> distanceHeuristics vn state
        | _ -> 0
    

    let generateGoalCondition goal =
        match goal with
        | At vertex -> fun state ->  state.Self.Node = vertex
        | Explored vertex -> fun state -> Set.contains vertex state.ExploredNodes 
        | Attacked agent -> agentAttacked agent
        | Probed vertex -> vertexProbed vertex
        | Inspected vertex -> vertexInspected vertex
        | Parried -> parried
        | Charged charge -> charged charge
        | AtMinValueNodeNotPartOfZone value -> atMinValueNodeNotPartOfZone value
        | Repaired agent -> agentRepaired agent
        | GetCloseTo agent -> getCloseTo agent
        | Surveyed -> surveyedNeighbourEdges
        | KillAll vertex -> killAll vertex



    let availableActions state  goal  =
        let actions = 
            match goal with
            | At _ | Explored _ | AtMinValueNodeNotPartOfZone _ | GetCloseTo _-> 
                gotoActions state
            | Attacked agent -> 
                gotoActions state @ attackActions state agent
            | Inspected vertex -> 
                gotoActions state @ inspectActions state vertex
            | Probed vertex -> 
                gotoActions state @ probeActions state vertex 
            | Repaired agent -> 
                gotoActions state @ repairActions state agent 
            | Parried -> 
                parryActions state
            | Charged _ -> 
                rechargeActions state
            | Surveyed ->
                [surveyAction]
            | KillAll vertex ->
                let agentsAtVertex = agentsAtPos state.EnemyData vertex
                let agentNames = List.map getAgentName agentsAtVertex
                gotoActions state @ List.collect (attackActions state) agentNames
        rechargeAction :: actions

    let goalHeuristics goal =
        match goal with
        | KillAll vertex 
        | At vertex 
        | Explored vertex
        | Probed vertex
        | Inspected vertex -> 
            distanceHeuristics vertex
        
        | GetCloseTo agent
        | Attacked agent 
        | Repaired agent ->
            agentDistanceHeuristics agent

        | Charged _
        | AtMinValueNodeNotPartOfZone _
        | Surveyed
        | Parried-> fun _ -> 0


    let goalVertex goal state =
        match goal with
        | KillAll vertex
        | At vertex 
        | Explored vertex
        | Inspected vertex
        | Probed vertex ->
            Some <| vertex
        
        | Attacked agent 
        | Repaired agent ->
            agentAt agent state

        | _ -> None
