namespace NabfAgentLogic

module Repairer =

    open AgentTypes
    open AgentLogicLib
    open RepairerLogic

    let getRepairerTree : Decision<(State -> (bool*Option<Action>))> =
        Options 
            [
                Choice(repairNearbyAgent)
                Choice(workOnRepairGoal)
            ]
