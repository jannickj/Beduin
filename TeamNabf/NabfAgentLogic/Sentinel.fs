namespace NabfAgentLogic

module Sentinel =

    open AgentTypes
    open AgentLogicLib
    open SentinelLogic

    let getSentinelTree : Decision<(State -> (bool*Option<Action>))> =
        Options 
            [
            ]