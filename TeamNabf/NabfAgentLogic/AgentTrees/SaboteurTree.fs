namespace NabfAgentLogic
module SaboteurTree =

    open FsPlanning.Agent.Planning
    open Saboteur
    open AgentTypes

    let getSaboteurDesires : DesireTree<State,Intention> = 
            ManyDesires 
                [
                    Desire(applyToAttackJob)

                    Desire(spontanouslyAttackAgentOnMyNode)

                    Desire(workOnAttackJob)

                    Desire(spontanouslyAttackAgent)

                    Desire(applyToDisruptJob)

                    Desire(workOnDisruptJobThenParryIfEnemiesClose)

                    Desire(findAgentToDestroy)
                ]