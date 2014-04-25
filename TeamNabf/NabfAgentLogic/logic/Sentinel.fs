namespace NabfAgentLogic
module Sentinel =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib

    ///////////////////////////////////Helper functions//////////////////////////////////////
    let calculateDesireOccupyJob (j:Job) (s:State) = 
        let ((_,value,_,_),_) = j
        value
   

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let applyToOccupyJob (s:State) = 
        let applicationList = createApplicationList s JobType.AttackJob calculateDesireOccupyJob
        Some(
                "apply to all occupy jobs"
                , Communication
                , [Plan(fun state -> applicationList)]
            )
    
    let doOccupyJobThenParryIfEnemiesClose (s:State) = None             
    
    let applyToDisruptJob (s:State) = None
    
    let doDisruptJobThenParryIfEnemiesClose (s:State) = None