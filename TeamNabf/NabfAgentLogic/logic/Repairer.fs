namespace NabfAgentLogic
module Repairer =

    open FsPlanning.Agent.Planning
    open AgentTypes

    let getRepairerDesires : DesireTree<State,Intention> = Desire (fun s -> None)