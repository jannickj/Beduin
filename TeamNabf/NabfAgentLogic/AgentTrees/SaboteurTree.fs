namespace NabfAgentLogic
module SaboteurTree =

    open FsPlanning.Agent.Planning
    open Saboteur
    open AgentTypes
    open Common

    let getSaboteurDesires : DesireTree<State,Intention> = 
            ManyDesires 
                [
                    Desire(haveRangeTwo)

                    Desire(unapplyFromJobsWhenDisabled)
                                       
                    Desire(killAgentICanSee)

                    Desire(findAgentToDestroy)
                ]