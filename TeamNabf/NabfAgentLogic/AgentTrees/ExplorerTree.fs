namespace NabfAgentLogic
module ExplorerTree =

    open FsPlanning.Agent.Planning
    open Explorer
    open AgentTypes

    let getExplorerDesires : DesireTree<State,Intention> = 
            ManyDesires 
                [
                    Desire(findNewZone)

                    Desire(findNodeToProbePhase1)

                    Desire(applyToOccupyJob)

                    Desire(workOnOccupyJob)

                    Desire(findNodeToProbeUnconditional)
                ]