namespace NabfAgentLogic
module Inspector =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants
    open Graphing.Graph
    open GeneralLib

    let distanceToOccupyJobMod = 0.1

    ///////////////////////////////////Helper functions//////////////////////////////////////
    let calculateDesireOccupyJob (j:Job) (s:State) = 
        let ((_,newValue,_,_),(jobData)) = j      
        let oldJobValue = 
                            if (s.MyJobs.IsEmpty) then
                                0
                            else
                                (getJobValueFromJoblist s.MyJobs s)

        let jobTargetNode = 
            match jobData with
            | OccupyJob (_,zone) -> zone.Head
        

        let distanceToJob = (getDistanceToJobAndNumberOfEnemyNodes jobTargetNode s)
        
        let personalValueMod = 1 |> float//if an agent has some kind of "personal" preference 
                                         //that modifies how much it desires the new job, using the input modifier 

        
        //final desire
        int <| (((float newValue) * personalValueMod) - (float oldJobValue)) + (-(distanceToJob * DISTANCE_TO_OCCUPY_JOB_MOD)) + INSPECTOR_OCCUPYJOB_MOD
   

    let nodeHasUninspectedAgent (state:State) node =
        List.exists (fun a -> a.Role.IsNone && a.Node = node) state.EnemyData

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let spontaneousInspectAgentOnMyNode (inputState : State) =
        let vertex = inputState.Self.Node
        if List.isEmpty <| List.filter (fun enemy -> Option.isNone enemy.Role) (enemiesHere inputState vertex) then
            None
        else 
            Some <| normalIntention ("inspect vertex " + vertex, Activity, [Requirement <| Inspected vertex])
    
    let spontanousInspectAgent (inputState:State) = 
        let rec nodeToAgentCount map list = 
            match list with
            | agent :: other when Option.isNone agent.Role -> 
                let tmpMap = (nodeToAgentCount map other)
                let count = if Map.containsKey agent.Node tmpMap then tmpMap.[agent.Node] + 1 else 1
                Map.add agent.Node count tmpMap
            | agent :: other ->
                nodeToAgentCount map other
            | [] -> 
                map

        let bestVertex = 
            let mapping = nodeToAgentCount Map.empty (nearbyEnemies inputState inputState.Self)
            if Map.isEmpty mapping then
                None
            else
                Map.toList mapping
                |> List.maxBy snd
                |> fst
                |> Some
            
        match bestVertex with
        | Some vertex -> Some <| normalIntention ("inspect vertex " + vertex, Activity, [Requirement <| Inspected vertex])
        | None -> None
        
//        let uninspectedNearbyEnemies = List.filter (fun a -> a.Role.IsNone) (nearbyEnemies inputState inputState.Self)
//        match uninspectedNearbyEnemies with
//        | [] -> None
//        | head::tail ->     
//            Some(
//                    "inspect agent " + head.Name
//                    , Activity
//                    , [Requirement (Inspected head.Name)]
//                )

    let applyToOccupyJob (inputState:State) = 
        let applicationList = createApplicationList inputState JobType.OccupyJob calculateDesireOccupyJob
        Some(
                "apply to all occupy jobs"
                , Communication
                , [Plan(fun state -> Some applicationList)]
            )
    
    let applyToDisruptJob (inputState:State) = None //advanced feature
    
    let workOnDisruptJob (inputState:State) = None //advanced feature
    
    let findAgentToInspect (inputState:State) = 
        let neighbourIds = (getNeighbourIds inputState.Self.Node inputState.World)           
        let neighbours = 
                            if (neighbourIds.Length = 1) then
                                neighbourIds
                            else
                                List.filter ((<>) inputState.LastPosition) neighbourIds
        let rand = System.Random()
        let index = rand.Next(0, List.length neighbours)
        let target = List.nth neighbours index
        Some <| normalIntention 
                    (   "go to node " +  target
                    ,   Activity
                    ,   [Requirement (At target)]
                    )