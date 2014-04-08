namespace NabfAgentLogic

module Inspector =

    open AgentTypes
    open AgentLogicLib
    open InspectorLogic

    let getInspectorTree : Decision<(State -> (bool*Option<Action>))> =
        Options 
            [
                Choice(inspect)
            ]