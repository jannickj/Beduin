namespace NabfAgentLogic

module AgentTypes =

    open Graphing.Graph

    type TeamName = string
    type AgentName = string

    //To be done
    type Intention = string
    type Solution = string

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
        }
    
    type Command =
        | Goto of VertexName

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

    type Job = JobHeader * JobData

    type SeenVertex = VertexName * TeamName option
    type AgentRolePercept = AgentName * AgentRole * int

    type SimStartData =
        { SimId          :   int
        ; SimEdges       :   int
        ; SimVertices    :   int
        ; SimRole        :   AgentRole
//        ; SimTotalSteps  :   int
        }
    
    type JobPercept =
        | AddedOrChangedJob of Job
        | RemovedJob of Job
        | AcceptedJob of JobID*VertexName
        | FiredFrom of JobID 

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
        | NewRoundPercept
        | AgentRolePercept  of AgentRolePercept
        | JobPercept        of JobPercept

    type SimulationID = int

    type CommunicationAction =
        | CreateJob of Job
        | RemoveJob of JobID
        | UpdateJob of Job
        | ApplyJob of JobID*Desirability
        | UnapplyJob of JobID
        | SimulationSubscribe
        | ShareKnowledge of Percept list
        | NewRound of int
    
    type AgentAction = 
        | Communicate of CommunicationAction
        | Perform of Action

    type SendMessage = SimulationID * CommunicationAction

    type Deadline = uint32
    type CurrentTime = uint32
    type Rank = int
    type Score = int
    type ActionID = int
    type ActionRequestData = Deadline * CurrentTime * ActionID
    
    

    type AgentServerMessage =
        | JobMessage of JobPercept
        | SharedPercepts of Percept list
        | RoundChanged of int

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
            LastPosition     : VertexName
            NewVertices      : SeenVertex list
            NewEdges         : Edge list
            LastStepScore    : int
            Money            : int
            Score            : int
            ThisZoneScore    : int
            LastActionResult : ActionResult
            LastAction       : Action
            TeamZoneScore    : int
            Jobs             : Job list
        }

    type OptionFunc = State -> (bool*Option<Action>)

    type DecisionRank = int
