namespace NabfAgentLogic
module RepairerTree =

    open FsPlanning.Agent.Planning
    open Repairer
    open AgentTypes

    let getRepairerDesires : DesireTree<State,Intention> = 
            ManyDesires 
                [
                    Desire(spontanouslyRepairNearbyDamagedAgent)

                    Desire(applyToRepairJob)
                    Desire(giveMyLocationToRepairee)
                    Desire(workOnRepairJob)
                ]