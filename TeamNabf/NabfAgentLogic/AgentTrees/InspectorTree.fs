namespace NabfAgentLogic
module InspectorTree =

    open FsPlanning.Agent.Planning
    open Inspector
    open AgentTypes
    open Common
    open Constants

    let getInspectorDesires : DesireTree<State,Intention> = 
            ManyDesires 
                [
                    Desire(spontanousInspectAgent)

                    Desire(applyToOccupyJob INSPECTOR_OCCUPYJOB_MOD)

                    Desire(workOnOccupyJob)//Heuristic

                    Desire(applyToDisruptJob)

                    Desire(workOnDisruptJob)//Heuristic

                    Desire(findAgentToInspect)
                ]