namespace NabfAgentLogic
module InspectorTree =

    open FsPlanning.Agent.Planning
    open Inspector
    open AgentTypes

    let getInspectorDesires : DesireTree<State,Intention> = 
            ManyDesires 
                [
                    Desire(spontanousInspectAgent)

                    Desire(applyToOccupyJob)

                    Desire(doOccupyJob)

                    Desire(applyToDisruptJob)

                    Desire(doDisruptJob)

                    Desire(findAgentToInspect)
                ]