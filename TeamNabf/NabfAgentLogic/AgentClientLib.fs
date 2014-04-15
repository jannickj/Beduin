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

    let parseIilPercepts (perceptCollection:IilPerceptCollection) : ServerMessage =
            let percepts = parsePerceptCollection perceptCollection
            parseIilServerMessage percepts

    let buildIilSendMessage ((id,act):SendMessage) =
        IiLang.IiLangDefinitions.buildIilAction (IiLang.IilTranslator.buildIilMetaAction act id)

    let buildInitState (name, simData:SimStartData) =
            {   World = Map.empty
            ;   Self =  {   Energy = Some 0
                        ;   MaxEnergy = Some 0
                        ;   Health = Some 0
                        ;   MaxHealth = Some 0
                        ;   Name = name
                        ;   Node = ""
                        ;   Role = Some (simData.SimRole)
                        ;   Strength = Some 0
                        ;   Team = OUR_TEAM
                        ;   Status = Normal
                        ;   VisionRange = Some 0
                        }
            ;   FriendlyData = []
            ;   EnemyData = List.Empty
            ;   SimulationStep = 0
            ;   LastPosition = ""
            ;   NewVertices = []
            ;   NewEdges = []
            ;   LastStepScore = 0
            ;   LastAction = Skip
            ;   LastActionResult = Successful
            ;   Money = 0
            ;   Score = 0
            ;   TeamZoneScore = 0
            ;   ThisZoneScore = 0
            ;   Jobs = []
            } : State