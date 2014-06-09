namespace NabfAgentLogic
module ExplorerTree =

    open FsPlanning.Agent.Planning
    open Explorer
    open AgentTypes
    open Common
    open Constants

    let getExplorerDesires : DesireTree<State,Intention> = 
            ManyDesires 
                [
                    Desire(workOnOccupyJob)

                    Desire(findNewZone)
                    
                    Conditional
                        (   inPhase1,
                            ManyDesires
                                [
                                    Desire findNodeToProbe
                                    Desire(applyToOccupyJob EXPLORER_OCCUPYJOB_MOD)
                                ]
                        )


                    Desire(findNodeToProbe)
                ]