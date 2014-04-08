namespace NabfAgentLogic

module DecisionTree =
    
    open AgentTypes
    open CommonLogic

    open SharedLogic
    open Saboteur
    open Explorer
    open Inspector
    open Sentinel
    open Repairer

    let isRole (r:Option<AgentRole>)(s:State) = (s.Self.Role = r,None)

    let isWorkingOnOccupyGoal state = 
        match List.tryFind (function | JobGoal (OccupyGoal _) -> true | _ -> false) state.Goals with
        | Some _ -> (true, None)
        | None _ -> (false, None)

    let getRoleDecision : Decision<(State -> (bool*Option<Action>))> =
        Options 
            [
                Condition(isRole (Some Explorer),
                    Options [ getExplorerTree ])
                Condition(isRole (Some Repairer),
                    Options [ getRepairerTree ])
                Condition(isRole (Some Saboteur),
                    Options [ getSaboteurTree ])
                Condition(isRole (Some Inspector),
                    Options [ getInspectorTree ])
                Condition(isRole (Some Sentinel),
                    Options [ getSentinelTree ])
            ]

    let getTree : Decision<(State -> (bool*Option<Action>))> =
        Options 
            [
                getRoleDecision

//                Condition (isWorkingOnOccupyGoal,
//                    Options [ Choice workOnKiteGoal ])

                Choice workOnOccupyGoal

                //Choice(reactToEnemyAgent)

                Choice(exploreLocalGraph)

                Choice(idle)
            ]