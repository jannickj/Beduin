namespace NabfAgentLogic
    module DebuggingFeatures = 
        open AgentTypes
        open CommonLogic
        open Graphing.Graph
        open Graphing.Dijkstra
        open PathFinding
        open AgentLogicLib

        let AccomplishGotoGoal (s:State) =
            let gotoGoal = List.tryFind (fun g -> match g with | GotoGoal _ -> true | _ -> false) s.Goals
            match gotoGoal with
            | Some (GotoGoal vertex) -> pathingTryGo vertex s
            | _ -> (false,None)

        let moveTo (vName:string) (s:State) = 
            //(tryGo s.World.[s.Self.Node] s)
            (true,Some (Goto s.Self.Node))
//            if s.Self.Node = vName then
//                (true,Some Skip)
//            elif s.World.ContainsKey(vName) then
//                let p = pathTo s.Self vName s.World
//                if p.IsSome && (not p.Value.IsEmpty) then
//                    tryGo s.World.[p.Value.Head] s
//                else
//                    (false,None)
//            else
//                (false,None)

        let moveToDTree vName = 
            Options 
                    [   Choice(moveTo vName)
                    ;   Choice(exploreLocalGraph)
                    ]

        let debugModeTree = 
            Options 
                    [   Choice(AccomplishGotoGoal)
                    ;   Choice(exploreLocalGraph)
                    ]

        
