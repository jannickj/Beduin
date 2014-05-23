namespace NabfAgentLogic
module Explorer =

    open FsPlanning.Agent.Planning
    open AgentTypes

    let getExplorerDesires : DesireTree<State,Intention> = Desire (fun s -> None)