namespace NabfAgentLogic
module SaboteurTree =

    open FsPlanning.Agent.Planning
    open Saboteur
    open AgentTypes
    open Common

    let getSaboteurDesires : DesireTree<State,Intention> = 
            ManyDesires 
                [
                    Desire(unapplyFromJobsWhenDisabled)

                    Desire(applyToAttackJob)

                    Desire(spontanouslyAttackAgentOnMyNode)

                    //Desire(spontanouslyAttackAgent)
                    Desire(killAgentICanSee)
                    Desire(workOnAttackJob)                    

                    Desire(applyToDisruptJob)

                    Desire(workOnDisruptJobThenParryIfEnemiesClose)


                    Desire(findAgentToDestroy)
                ]