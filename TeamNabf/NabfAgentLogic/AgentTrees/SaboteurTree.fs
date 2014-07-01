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

                    //Desire(spontanouslyAttackAgent)
                    //this will easily result in our saboteurs following a random agent when he goes from 1 job to another. 
                    Desire(workOnAttackJob)
                    Desire(killAgentICanSee)  
                    //Desire(spontanouslyAttackAgentOnMyNode)                      
                    //Desire(killAgentICanSee)                

                    //Desire(applyToDisruptJob)

                    //Desire(workOnDisruptJobThenParryIfEnemiesClose)

                    Desire(patrolAZone)

                    //Desire(findAgentToDestroy)
                ]