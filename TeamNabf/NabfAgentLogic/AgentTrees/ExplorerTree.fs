namespace NabfAgentLogic
module ExplorerTree =

    open FsPlanning.Agent.Planning
    open Explorer
    open AgentTypes

    let getExplorerDesires : DesireTree<State,Intention> = 
            ManyDesires 
                [
                    Desire(findNewZone)

                    Conditional(,Desire(findNodeToProbe))

                    Desire(applyToOccupyJob)

                    Desire(workOnOccupyJob)

                    Desire(findNodeToProbe)

                    Desire
                ]