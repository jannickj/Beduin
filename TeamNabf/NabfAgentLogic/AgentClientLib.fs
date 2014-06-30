namespace NabfAgentLogic
module AgentClientLib =
    open System
    open Graphing.Graph
    open JSLibrary.IiLang
    open JSLibrary.IiLang.DataContainers
    open AgentTypes
    open IiLang.IiLangDefinitions
    open IiLang.IilTranslator
    open Logging
    open Constants
    open GeneralLib

    let parseIilPercepts (perceptCollection:IilPerceptCollection) : ServerMessage =
            let percepts = parsePerceptCollection perceptCollection
            parseIilServerMessage percepts

    let buildIilSendMessage ((id,act):SendMessage) =
        IiLang.IiLangDefinitions.buildIilAction (IiLang.IilTranslator.buildIilMetaAction act id)


    let buildState name team = 
        {   World = Map.empty<string, Vertex>
        ;   Self =  buildAgent name team true
        ;   FriendlyData = []
        ;   EnemyData = List.Empty
        ;   SimulationStep = 0
        ;   LastPosition = ""
        ;   LastStepScore = 0
        ;   Score = 0
        ;   ThisZoneScore = 0
        ;   LastActionResult = Successful
        ;   LastAction = Skip
        ;   TeamZoneScore = 0
        ;   Jobs = []
        ;   MyJobs = []
        ;   TotalNodeCount = 0
        ;   NewKnowledge = []
        ;   PlannerProbed = Set.empty
        ;   PlannerRepairedAgents = Set.empty
        ;   PlannerInspectedEnemies = Set.empty
        ;   PlannerDisabledEnemies = Set.empty
        ;   GraphHeuristic = (Map.empty,Map.empty)
        ;   Relations = Map.empty
        ;   NodesControlledByEnemy = Set.empty
        ;   NodesInVisionRange = Set.empty
        ;   ExploredNodes = Set.empty
        ;   LastRoundState = None
        ;   Money = 0
        } : State

    let buildInitState name team (simData:SimStartData) =
        let state = buildState name team
        { state with Self = { state.Self with Role = Some simData.SimRole; RoleCertainty = 100}; TotalNodeCount = simData.SimVertices; NewKnowledge = [AgentRolePercept(name,team,simData.SimRole,100)] }
