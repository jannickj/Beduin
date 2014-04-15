namespace NabfAgentLogic
module Inspector =

    open FsPlanning.Agent.Planning
    open AgentTypes

    let getInspectorDesires : DesireTree<State,Intention> = Desire (fun s -> None)