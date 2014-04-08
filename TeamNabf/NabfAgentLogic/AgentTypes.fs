namespace NabfAgentLogic

module AgentTypes =

    open Graphing.Graph

    type TeamName = string
    type AgentName = string

    type Decision<'a> =
        | Condition of 'a * Decision<'a>
        | Choice of 'a
        | Options of Decision<'a> list

    type Upgrade =
        | Battery
        | Sensor
        | Shield
        | SabotageDevice

    type ActionResult =
        | Successful
        | Failed
        | FailedResources
        | FailedAttacked
        | FailedParried
        | FailedUnreachable
        | FailedOutOfRange
        | FailedInRange
        | FailedWrongParam 
        | FailedRole
        | FailedStatus
        | FailedLimit
        | FailedRandom

    type AgentRole =
        | Saboteur
        | Explorer
        | Repairer
        | Inspector
        | Sentinel

    type Level = int

    type Achievement =
        | ConqueredZone      of Level
        | ProbedVertices     of Level
        | SurveyedEdges      of Level
        | InspectedVehicles  of Level
        | Attacked           of Level
        | Parried            of Level
    
    type EntityStatus =
        | Normal
        | Disabled

    type Agent =
        { Energy      : Option<int>
        ; Health      : Option<int>
        ; MaxEnergy   : Option<int>
        ; MaxHealth   : Option<int>
        ; Name        : string
        ; Node        : string
        ; Role        : Option<AgentRole>
        ; Strength    : Option<int>
        ; Team        : string
        ; VisionRange : Option<int>
        ; Status      : EntityStatus
        }

    type TeamState =
        { LastStepScore : int
        ; Money         : int
        ; Score         : int
        ; ZoneScore     : int
        ; Achievements  : Achievement list
        }

    type Action =
        | Skip
        | Recharge
        | Goto      of VertexName
        | Probe     of Option<VertexName>
        | Survey    
        | Inspect   of Option<AgentName>
        | Attack    of AgentName
        | Parry
        | Repair    of AgentName
        | Buy       of Upgrade

    

    type JobID = int
    type JobValue = int
    type Desirability = int

    type JobType = 
        | EmptyJob = 0
        | OccupyJob = 1
        | RepairJob = 2
        | DisruptJob = 3
        | AttackJob = 4

    type JobData =
        | OccupyJob of VertexName list * VertexName list
        | RepairJob of VertexName * AgentName
        | DisruptJob of VertexName
        | AttackJob of VertexName list //Change to single vertex?
        | EmptyJob
    
    type AgentsNeededForJob = int

    type JobHeader = Option<JobID> * JobValue * JobType * AgentsNeededForJob
    type JobGoal =
        | OccupyGoal of VertexName
        | RepairGoal of VertexName * AgentName
        | DisruptGoal of VertexName
        | AttackGoal of VertexName

    type Goal =
        | JobGoal  of JobGoal
        | KiteGoal of int * (Agent list)
        | GotoGoal of VertexName

    type Job = JobHeader * JobData


    let levelToPoints start level =
        start * (pown 2 level)

    let achievementPoints achievement =
        levelToPoints <||
            match achievement with
            | ConqueredZone l     -> (l, 10)
            | ProbedVertices l    -> (l, 5)
            | SurveyedEdges l     -> (l, 10)
            | InspectedVehicles l -> (l, 5)
            | Attacked l          -> (l, 5)
            | Parried  l          -> (l, 5)

    type SeenVertex = VertexName * TeamName option
    type AgentRolePercept = AgentName * AgentRole * int

    type Percept =
        | EnemySeen         of Agent
        | VertexSeen        of SeenVertex
        | VertexProbed      of VertexName * int
        | EdgeSeen          of Edge
        | SimulationStep    of int
        | MaxEnergyDisabled of int
        | LastAction        of Action
        | LastActionResult  of ActionResult
        | ZoneScore         of int
        | Team              of TeamState
        | Self              of Agent
        | AgentRolePercept  of AgentRolePercept
        

    type SimulationID = int

    type MetaAction =
        | CreateJob of Job
        | RemoveJob of JobID
        | UpdateJob of Job
        | ApplyJob of JobID*Desirability
        | UnapplyJob of JobID
        | SimulationSubscribe
        | ShareKnowledge of Percept list
        | NewRound of int
    
    type SendMessage = SimulationID * MetaAction

    type Deadline = uint32
    type CurrentTime = uint32
    type Rank = int
    type Score = int
    type ActionID = int
    type ActionRequestData = Deadline * CurrentTime * ActionID
    
    type SimStartData =
        { SimId          :   int
        ; SimEdges       :   int
        ; SimVertices    :   int
        ; SimRole        :   AgentRole
//        ; SimTotalSteps  :   int
        }

    type AgentServerMessage =
        | AddedOrChangedJob of Job
        | RemovedJob of Job
        | AcceptedJob of JobID*VertexName
        | SharedPercepts of Percept list
        | RoundChanged of int
        | FiredFrom of JobID

    type MarsServerMessage =  
        | ActionRequest of ActionRequestData * Percept list
        | SimulationStart of SimStartData
        | SimulationEnd of Rank * Score
        | ServerClosed

    type ServerMessage = 
        | AgentServerMessage of AgentServerMessage
        | MarsServerMessage of MarsServerMessage

    type State =
        { 
            World            : Graph; 
            Self             : Agent; 
            FriendlyData     : Agent list;         
            EnemyData        : Agent list; 
            SimulationStep   : int;
            NearbyAgents     : Agent list
            OwnedVertices    : Map<VertexName, TeamName>
            LastPosition     : VertexName
            NewVertices      : SeenVertex list
            NewEdges         : Edge list
            LastStepScore    : int
            Money            : int
            Score            : int
            ThisZoneScore    : int
            Achievements     : Achievement list
            LastActionResult : ActionResult
            LastAction       : Action
            TeamZoneScore    : int
            NewZone          : Option<Graph * bool>
            NewZoneFrontier  : VertexName list
            Goals            : Goal list
            Jobs             : Job list
        }

    type OptionFunc = State -> (bool*Option<Action>)

    type DecisionRank = int
