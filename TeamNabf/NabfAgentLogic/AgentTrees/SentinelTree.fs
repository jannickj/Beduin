namespace NabfAgentLogic
module SentinelTree =

    open FsPlanning.Agent.Planning
    open Sentinel
    open AgentTypes
    open Common

    let getSentinelDesires : DesireTree<State,Intention> = 
            ManyDesires 
                [
                    Desire(applyToOccupyJob)

                    //Desire(workOnOccupyJobThenParryIfEnemiesClose)

                    Desire(workOnOccupyJob)

                    Desire(applyToDisruptJob)

                    Desire(workOnDisruptJobThenParryIfEnemiesClose)
                ]