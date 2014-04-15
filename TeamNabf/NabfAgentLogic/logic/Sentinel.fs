namespace NabfAgentLogic
module Sentinel =

    open FsPlanning.Agent.Planning
    open AgentTypes

    let getSentinelDesires : DesireTree<State,Intention> = Desire (fun s -> None)