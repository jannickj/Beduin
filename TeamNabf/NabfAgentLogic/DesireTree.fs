namespace NabfAgentLogic

module DesireTree =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open Common
    open ExplorerTree
    open RepairerTree
    open SentinelTree
    open SaboteurTree
    open InspectorTree

    let isRole (r:Option<AgentRole>)(s:State) = s.Self.Role = r

    let getRoleDesires : DesireTree<State,Intention> =
        ManyDesires 
            [
                Conditional(isRole (Some Explorer),
                    ManyDesires [ getExplorerDesires ])
                Conditional(isRole (Some Repairer),
                    ManyDesires [ getRepairerDesires ])
                Conditional(isRole (Some Saboteur),
                    ManyDesires [ getSaboteurDesires ])
                Conditional(isRole (Some Inspector),
                    ManyDesires [ getInspectorDesires ])
                Conditional(isRole (Some Sentinel),
                    ManyDesires [ getSentinelDesires ])
            ]

    let getTree : DesireTree<State,Intention> =
            ManyDesires 
                [
                    Desire(shareKnowledge)
                    Desire(onlyOneJob)

                    ////Desire(postDefenceJob)

                    ////Desire(postAttackJob)

                    Desire(getRepaired)

                    ////Desire(postDisruptJob)

                    getRoleDesires

                    Desire(exploreMap)

                    Desire(generateMinimumValue)
                ]
