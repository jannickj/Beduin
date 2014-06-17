namespace NabfAgentLogic
module Sentinel =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants

    ///////////////////////////////////Helper functions//////////////////////////////////////
    
   

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let workOnOccupyJobThenParryIfEnemiesClose (inputState:State) = 
        let myJobs = List.map (fun (id,_) -> getJobFromJobID inputState id) inputState.MyJobs
        let myOccupyJobs = getJobsByType JobType.OccupyJob myJobs
        match myOccupyJobs with
        | ((id,_,_,_),_)::_ -> 
            let (_,node) = List.find (fun (jid,_) -> id.Value = jid) inputState.MyJobs
            Some <| normalIntention 
                (   "occupy node " + node + " and then parry"
                ,   Activity
                ,   [ Requirement (At node)
                    ; Plan <| fun _ -> Some [Perform Recharge]
                    ]
                )
        | [] -> None   
    
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