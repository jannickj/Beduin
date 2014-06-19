namespace AgentLogicTest
module StructureBuilder =
    open NabfAgentLogic.AgentTypes
    open NabfAgentLogic.Search.HeuristicDijkstra

    let buildAgentWithRole name team node role =
        { Energy = None
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
        {   World = world
            ;   Self =  {   Energy = Some energy                        
                        ;   Health = Some 0
                        ;   MaxEnergy = Some 30
                        ;   MaxEnergyDisabled = Some 30
                        ;   MaxHealth = Some 0
                        ;   Name = "gunner"
                        ;   Node = node
                        ;   Role = Some role
                        ;   RoleCertainty = 100
                        ;   Strength = Some 0
                        ;   Team = "Team Love Unit testing"
                        ;   Status = Normal
                        ;   VisionRange = Some 0
                        }
            ;   FriendlyData = []
            ;   EnemyData = List.Empty
            ;   InspectedEnemies = Set.empty
            ;   SimulationStep = 0
            ;   LastPosition = ""
//            ;   NewVertices = []
            ;   NewEdges = []
            ;   LastStepScore = 0
            ;   Score = 0
            ;   ThisZoneScore = 0
            ;   LastActionResult = Successful
            ;   LastAction = Skip
            ;   TeamZoneScore = 0
            ;   Jobs = []
            ;   MyJobs = []
            ;   TotalNodeCount = List.length <| Map.toList world
            ;   MyExploredCount = 0
            ;   ProbedCount = 0  
            ;   NewKnowledge = []
            ;   PlannerProbed = Set.empty
            ;   PlannerRepairedAgents = Set.empty
            ;   PlannerInspectedEnemies = Set.empty
            ;   PlannerDisabledEnemies = Set.empty
            ;   GraphHeuristic = (Map.empty<string*string, _>,Map.empty)
            ;   MailsReceived = Map.empty
            ;   Relations = Map.empty
            ;   NodesControlledByEnemy = Set.empty
            } : State

    let buildState node role world = buildStateWithEnergy node role world 30 

    let enhanceStateWithGraphHeuristics state =
        List.fold updateHeuristic state (List.map fst <| Map.toList state.World)
