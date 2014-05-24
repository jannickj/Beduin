namespace NabfAgentLogic
module InspectorTree =

    open FsPlanning.Agent.Planning
    open Inspector
    open AgentTypes
    open Common

    let getInspectorDesires : DesireTree<State,Intention> = 
            ManyDesires 
                [
                    Desire(spontanousInspectAgent)

                    Desire(applyToOccupyJob)

                    Desire(workOnOccupyJob)

                    Desire(applyToDisruptJob)

                    Desire(workOnDisruptJob)

                    Desire(findAgentToInspect)
                ]