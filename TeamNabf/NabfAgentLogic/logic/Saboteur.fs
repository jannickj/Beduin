namespace NabfAgentLogic
module Saboteur =

    open FsPlanning.Agent.Planning
    open AgentTypes

    let getSaboteurDesires : DesireTree<State,Intention> = Desire (fun s -> None)