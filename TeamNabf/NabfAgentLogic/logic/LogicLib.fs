namespace NabfAgentLogic
module LogicLib =
    
    open FsPlanning.Agent.Planning
    open AgentTypes

    let isPartOfOccupyJob n (s:State) = List.exists (fun (j:Job) -> j ) s.Jobs