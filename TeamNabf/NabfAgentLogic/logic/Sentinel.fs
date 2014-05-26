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
            Some
                (   "occupy node " + node + "and then parry"
                ,   Activity
                ,   [
                        Requirement <| fun state -> state.Self.Node = node
                    ;   Requirement(
                                    fun state ->  
                                                    if (checkIfEnemyOnNode state state.Self.Node) then
                                                                match state.LastAction with
                                                                | (Parry _) -> true
                                                                | _ -> false
                                                    else
                                                                match state.LastAction with
                                                                | (Recharge _) -> true
                                                                | _ -> false
                                   )
                    ]
                )
        | [] -> None   
    
    let applyToDisruptJob (inputState:State) = None //advanced feature
    
    let workOnDisruptJobThenParryIfEnemiesClose (inputState:State) = None //advanced feature