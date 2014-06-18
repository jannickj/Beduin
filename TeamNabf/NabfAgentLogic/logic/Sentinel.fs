namespace NabfAgentLogic
module Sentinel =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants
    open Graphing
    open Logging

    ///////////////////////////////////Helper functions//////////////////////////////////////
   

    ////////////////////////////////////////Logic////////////////////////////////////////////


    let surveyIfNeeded (inputState:State) =      
        let rangeOneEdges = inputState.World.[inputState.Self.Node].Edges  
        let neightbours = neighbourNodes inputState inputState.Self
        let edgesOfNeightbours = List.concat <| List.map 
                                                    (
                                                    fun name -> Set.toList inputState.World.[name].Edges
                                                    ) neightbours

        let rangeTwoEdges = List.filter (fun s -> not (Set.contains s rangeOneEdges)) edgesOfNeightbours
        
        let rangeOneUnsurveyd = Set.filter (fun (value,_) -> Option.isNone value) rangeOneEdges
        let rangeTwoUnsurveyd = List.filter (fun (value,_) -> Option.isNone value) rangeTwoEdges

        let enoughEdgesInRange1 = (float rangeOneEdges.Count)*SURVEY_NEEDED_FACTOR_RANGE1 < (float rangeOneUnsurveyd.Count) 

        let enoughEdgesInRange1WhenConsideringRange2 = (float rangeOneEdges.Count)*SURVEY_NEEDED_FACTOR_RANGE1_WHILE_ON_2 < (float rangeOneUnsurveyd.Count) 
        let enoughEdgesInRange2 = (float rangeTwoEdges.Length)*SURVEY_NEEDED_FACTOR_RANGE2 < (float rangeTwoUnsurveyd.Length)

        if (enoughEdgesInRange1 || (enoughEdgesInRange2 && enoughEdgesInRange1WhenConsideringRange2)) then
                Some <| normalIntention 
                     ( "survey the area"
                     , Activity
                     , [Requirement <| Surveyed]
                     ) 
        else
            None 


    let workOnOccupyJobWithSurvey (inputState:State) = 
        logStateInfo inputState Intentions <| sprintf  "my jobs are: %A" (List.map fst inputState.MyJobs)
        let myJobs = List.map (fun (id,_) -> getJobFromJobID inputState id) inputState.MyJobs
        let myOccupyJobs = getJobsByType JobType.OccupyJob myJobs
        if (float (inputState.Self.Energy.Value) < float(inputState.Self.MaxEnergy.Value) * ENERGY_FACTOR_TO_PREFER_SURVEY_OVER_RECHARGE) then
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
        else
            match myOccupyJobs with
            | ((id,_,_,_),_)::_ -> 
                let (_,node) = List.find (fun (jid,_) -> id.Value = jid) inputState.MyJobs
                Some <| normalIntention 
                    ( "occupy node with survey " + node
                     , Activity
                     , [ Requirement (At node)
                       ; Plan <| fun _ -> Some [Perform Survey]
                       ]
                     )
            | [] -> None

//    let workOnOccupyJobThenParryIfEnemiesClose (inputState:State) = 
//        let myJobs = List.map (fun (id,_) -> getJobFromJobID inputState id) inputState.MyJobs
//        let myOccupyJobs = getJobsByType JobType.OccupyJob myJobs
//        match myOccupyJobs with
//        | ((id,_,_,_),_)::_ -> 
//            let (_,node) = List.find (fun (jid,_) -> id.Value = jid) inputState.MyJobs
//            Some <| normalIntention 
//                (   "occupy node " + node + " and then parry"
//                ,   Activity
//                ,   [ Requirement (At node)
//                    ; Plan <| fun _ -> Some [Perform Recharge]
//                    ]
//                )
//        | [] -> None   
    
    let applyToDisruptJob (inputState:State) = None //advanced feature
    
    let workOnDisruptJobThenParryIfEnemiesClose (inputState:State) = None //advanced feature

    let selfDefence (inputState:State) = 
        let agentList = List.filter (fun a -> a.Node = inputState.Self.Node) inputState.EnemyData
        let probableSaboteursOnNode = List.filter (fun a -> a.Role.IsSome && a.Role.Value = Saboteur && a.RoleCertainty >= 50) agentList
        let isInDanger = (List.length probableSaboteursOnNode) > 0
        match isInDanger with
        | true ->  Some <| normalIntention 
                       (   "defend myself from a saboteur on my node."
                       ,   Activity
                       ,   [ Plan <| fun _ -> Some [Perform Parry] ]
                       )
        | false -> None