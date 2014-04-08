namespace NabfAgentLogic

module Explorer =

    open AgentTypes
    open AgentLogicLib
    open ExplorerLogic

    let getExplorerTree : Decision<(State -> (bool*Option<Action>))> =
        Options 
            [
                Choice(probeVertex)
                Choice(exploreNewZone)
            ]

    
            