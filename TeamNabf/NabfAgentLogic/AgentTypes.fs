namespace NabfAgentLogic

module AgentTypes =

    open Graphing.Graph
    open Constants

    type TeamName = string
    type AgentName = string 

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
        ; MaxEnergyDisabled : Option<int>
        ; MaxHealth   : Option<int>
        ; Name        : string
        ; Node        : string
        ; Role        : Option<AgentRole>
        ; RoleCertainty : int //Percent chance that this is the role we think
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
        | OccupyJob of VertexName list * VertexName list //(agentPositions,zone)
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
        | KnowledgeSent     of Percept list
        | HeuristicUpdate   of VertexName * VertexName * (int*int)
        | CommucationSent   of CommunicationAction
        | JobPercept        of JobPercept
    and CommunicationAction =
        | CreateJob of Job
        | RemoveJob of JobID
        | UpdateJob of Job
        | ApplyJob of JobID*Desirability
        | UnapplyJob of JobID
        | SimulationSubscribe
        | ShareKnowledge of Percept list
        | NewRound of int

    type SimulationID = int
    
    
    
        
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

    type SubSetState =
        {
            Pos             : string
            HasEnergy       : bool
            PlannerProbed          : VertexName Set
            PlannerRepairedAgents  : AgentName Set
            PlannerInspectedEnemies : AgentName Set
            PlannerDisabledEnemies  : AgentName Set
        }

    [<CustomEquality>]
    [<CustomComparison>]
    type State =
        { 
            World            : Graph
            Self             : Agent
            FriendlyData     : Agent list     
            EnemyData        : Agent list
            InspectedEnemies : AgentName Set
            SimulationStep   : int
            LastPosition     : VertexName
            NewVertices      : SeenVertex list
            NewEdges         : Edge list
            LastStepScore    : int
            Score            : int
            ThisZoneScore    : int
            LastActionResult : ActionResult
            LastAction       : Action
            TeamZoneScore    : int
            Jobs             : Job list
            MyJobs           : (JobID * VertexName) list
            TotalNodeCount   : int
            NewKnowledge     : Percept list
            MyExploredCount  : int
            ProbedCount      : int
            GraphHeuristic   : (Map<VertexName*VertexName, (int*int)>* Map<VertexName,int>)

            ///USED FOR PLANNING ONLY DONT USE THEM IN INTENTION CHECKS
            PlannerProbed           : VertexName Set
            PlannerRepairedAgents   : AgentName Set
            PlannerInspectedEnemies : AgentName Set
            PlannerDisabledEnemies  : AgentName Set
        }           
        member self.GetSubSet =
            { 
                Pos = self.Self.Node; 
                HasEnergy = 
                    match self.Self.Energy with
                    | Some energy -> energy >= ACTION_COST_MAX
                    | _ -> false
                PlannerProbed = self.PlannerProbed
                PlannerRepairedAgents = self.PlannerRepairedAgents
                PlannerInspectedEnemies = self.PlannerInspectedEnemies
                PlannerDisabledEnemies = self.PlannerDisabledEnemies
            }
            
        override self.GetHashCode() = self.GetSubSet.GetHashCode()
        override self.Equals (other) = 
            match other with
            | :? State as o ->  o.GetSubSet = self.GetSubSet
            | _ -> false
        interface System.IComparable with
            member self.CompareTo yobj =
                match yobj with
                | :? State as o -> compare (o.GetSubSet) (self.GetSubSet)
                | _ -> failwith "fsharp sucks"

    type OptionFunc = State -> (bool*Option<Action>)

    type DecisionRank = int

    type IntentionType =
        | Communication
        | Activity
        | Inherent

    type Goal =
        | At        of VertexName
        | Probed    of VertexName
        | Attacked  of AgentName 
        | Repaired  of AgentName
        | Inspected of AgentName
        | Explored  of VertexName
        | AtMinValueNode of int
        | Parried
        | Charged   of int option

    type Objective = 
        | Plan of (State -> (AgentAction list) option)
        | Requirement of Goal
        | MultiGoal of (State -> Goal list)

    type Intention = string*IntentionType*(Objective list)
