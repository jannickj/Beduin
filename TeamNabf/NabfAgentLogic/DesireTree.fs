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
    let goto =  None // Some "v204"

    let selfNodeAndWorldExists (s:State) = s.World.IsEmpty |> not && s.Self.Node.Length > 0

    let getTree : DesireTree<State,Intention> =
            Conditional (selfNodeAndWorldExists,
                ManyDesires 
                    [
    //                    Desire  (
    //                                (fun s ->        
    //                                    if goto.IsSome && s.World.ContainsKey goto.Value then
    //                                            Some <| (   "Find a node"
    //                                                        ,   Activity
    //                                                        ,   [Requirement <| fun (s:State) -> s.Self.Node =  goto.Value]
    //                                                    )
    //                                    else
    //                                        None
    //                                )
    //                            )
                        Desire(shareKnowledge)
    
                        Desire(postDefenseJob)
    
                        Desire(postAttackJob)
    
    //                    ////Desire(postDisruptJob)
    //
                        Desire(getRepaired)
    //
                        getRoleDesires
    //
                        
                        Conditional
                            (   exploringNotDone,
                                Desire exploreMap
                            )
    //
                        Desire(generateSomeValue)
                        Desire(generateLittleValue)
                        Desire(generateLeastValue)
                        Desire(generateMinimumValue)
                    ]
                )