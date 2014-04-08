namespace NabfAgentLogic
module CommonLogic =

    open AgentTypes
    open Graphing.Graph
    open PathFinding
    open SaboteurLogic
    open RepairerLogic
    open SentinelLogic
    open ExplorerLogic
    open InspectorLogic
    open AgentLogicLib
    open Logging

    let reactToEnemyAgent (s:State) =
        let agents = List.partition (fun a -> (a.Node = s.Self.Node) && (a.Team <> s.Self.Team)) s.NearbyAgents
        if not (fst agents).IsEmpty then
            match s.Self.Role.Value with
            | Saboteur -> saboteurReact s agents
            | Repairer -> repairerReact s agents
            | Sentinel -> sentinelReact s agents
            | Explorer -> explorerReact s agents
            | Inspector -> inspectorReact s agents
        else
            (false,None)

    let exploreLocalGraph (s:State) =
        let rank = rankByType s

        if rank > 0 then
            logImportant (sprintf "%A Standing still due to rank %A" s.Self.Name rank)
            (false, None)
        else
            
            let unexplored = pathToNearestUnExplored s.Self s.World
            logImportant (sprintf "path to unexplored for %A: %A" s.Self.Name unexplored) 

            match unexplored with
            | Some (h :: t) -> tryGo s.World.[h] s
            | _ -> (false, None)
        
//        let rank = rankByType s
//        //Fix with rank
//        let unexplored = (pathsToNearestNUnexplored rank s.Self s.World)
//        
//
//        if unexplored = []
//        then 
//            (false,None) 
//        else
//            let index = rank % unexplored.Length 
//            logImportant (sprintf "path to unexplored: %A" unexplored.[index])     
//            tryGo s.World.[unexplored.[index].Head] s
            
    let idle (s:State) = recharge

