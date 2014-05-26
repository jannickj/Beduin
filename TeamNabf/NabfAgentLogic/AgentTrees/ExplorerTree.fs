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
                    Desire(findNewZone)
                    
                    Desire(applyToOccupyJob EXPLORER_OCCUPYJOB_MOD)
//                    Conditional
//                        (   inPhase1,
//                            ManyDesires
//                                [
//                                    Desire(findNodeToProbe)
//                                ]
//                        )

                    Desire(workOnOccupyJob)//Fix w. heuristic

                    Desire(findNodeToProbe)
                ]