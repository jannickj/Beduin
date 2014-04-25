namespace NabfAgentLogic
module SaboteurTree =

    open FsPlanning.Agent.Planning
    open Saboteur
    open AgentTypes

    let getSaboteurDesires : DesireTree<State,Intention> = 
            ManyDesires 
                [
                    Desire(applyToAttackJob)

                    Desire(doAttackJob)

                    Desire(spontanousAttackAgent)

                    Desire(applyToDisruptJob)

                    Desire(doDisruptJobThenParryIfEnemiesClose)

                    Desire(findAgentToDestroy)
                ]