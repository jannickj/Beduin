namespace AgentLogicTest
module StructureBuilder =
    open NabfAgentLogic.AgentTypes
    open NabfAgentLogic.Search.HeuristicDijkstra
    open NabfAgentLogic.AgentClientLib
    open NabfAgentLogic.GeneralLib

    let buildAgentWithRole name team node role =
        let agent = buildAgent name team true
        { agent with
            Energy = None
            ; Health = Some 30
            ; MaxEnergy = None
            ; MaxEnergyDisabled = None
            ; MaxHealth = None
            ; Name = name
            ; Node = node
            ; Role = role
            ; RoleCertainty = 100
            ; Strength = None
            ; Team = team
            ; Status = Normal
            ; VisionRange = None
        }

    let buildEnemyWithRole name node role =
        buildAgentWithRole name "EnemyTeam" node role

    let buildEnemy name node = buildEnemyWithRole name node None

    let buildStateWithEnergy node role world energy = 
        let state = buildState "" ""
        {  
            state with
                World = world
                ;   Self = { buildAgentWithRole "gunner" "Team Love Unit testing" node (Some role) with
                                    Energy = Some energy                        
                                ;   MaxEnergy = Some 30
                                ;   RoleCertainty = 100
                                ;   MaxEnergyDisabled = Some 300
                            }
               
                ;   TotalNodeCount = List.length <| Map.toList world
                
            } : State

    let buildState node role world = buildStateWithEnergy node role world 30 

    let enhanceStateWithGraphHeuristics state =
        List.fold (flip updateHeuristic) state (List.map fst <| Map.toList state.World)
