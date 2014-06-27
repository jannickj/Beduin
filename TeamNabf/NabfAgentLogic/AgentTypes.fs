namespace NabfAgentLogic

module AgentTypes =

    open Graphing.Graph
    open Constants
    open Logging

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
        | Useless

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
        { Energy            : Option<int>
        ; Health            : Option<int>
        ; MaxEnergy         : Option<int>
        ; MaxEnergyDisabled : Option<int>
        ; MaxHealth         : Option<int>
        ; Name              : string
        ; Node              : string
        ; Role              : Option<AgentRole>
        ; RoleCertainty     : int 
        ; Strength          : Option<int>
        ; Team              : string
        ; VisionRange       : Option<int>
        ; Status            : EntityStatus
        ; IsInVisionRange   : bool
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
        override self.ToString() = sprintf "%A" self


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
        | AttackJob of VertexName list * int //the int is a timestamp
        | EmptyJob
    
    type AgentsNeededForJob = int

    type JobHeader = Option<JobID> * JobValue * JobType * AgentsNeededForJob

    type Job = JobHeader * JobData

    type SeenVertex = VertexName * TeamName option

    type SimStartData =
        { SimId          :   int
        ; SimEdges       :   int
        ; SimVertices    :   int
        ; SimRole        :   AgentRole
        }
    
    type Message =
        | MyLocation of VertexName
        | GoingToRepairYou

    type RecipientName = AgentName
    type SenderName = AgentName
        //add new types here

    type Mail = SenderName*RecipientName*Message

    type JobPercept =
        | AddedOrChangedJob of Job
        | RemovedJob of Job
        | AcceptedJob of JobID*VertexName
        | FiredFrom of JobID 
    
    type RoundNumber = int
    type Percept =
        | VisibleEntity     of AgentName*TeamName*VertexName*EntityStatus
        | InspectedEntity   of Agent
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
        
        | NodeKnowledge     of (VertexName * (int option))
        | EdgeKnowledge     of Edge
        | AgentRolePercept  of AgentName * AgentRole * int
        | HeuristicUpdate   of VertexName * VertexName * (int*int)
        | CommucationSent   of CommunicationAction
        | MailPercept       of Mail
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
        | SendMail of Mail
    
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
    
    type Relation =
        | MyRepairer
    
    type SubSetState =
        {
            Pos             : string
            HasEnergy       : bool
            PlannerProbed          : VertexName Set
            PlannerRepairedAgents  : AgentName Set
            PlannerInspectedEnemies : AgentName Set
            PlannerDisabledEnemies  : AgentName Set
            EdgesOfCurrentPosSurveyed : bool
        }

    [<CustomEquality>]
    [<CustomComparison>]
    type State =
        { 
            World            : Graph
            Self             : Agent
            FriendlyData     : Agent list     
            EnemyData        : Agent list
            SimulationStep   : int
            LastPosition     : VertexName
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
            GraphHeuristic   : (Map<VertexName*VertexName, (int*int)>* Map<VertexName,int>)
            Relations        : Map<Relation,AgentName>
            NodesControlledByEnemy : VertexName Set
            NodesInVisionRange :VertexName Set
            ExploredNodes   : VertexName Set
            LastRoundState       : State option

            ///USED FOR PLANNING ONLY DONT USE THEM IN INTENTION CHECKS
            PlannerProbed          : VertexName Set
            PlannerRepairedAgents  : AgentName Set
            PlannerInspectedEnemies : AgentName Set
            PlannerDisabledEnemies : AgentName Set
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
                EdgesOfCurrentPosSurveyed =
                    let rangeOneEdges = self.World.[self.Self.Node].Edges          
                    0 = (Set.count <| Set.filter (fun (value,_) -> Option.isNone value) rangeOneEdges)
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
        | Inspected of VertexName
        | Explored  of VertexName
        | AtMinValueNodeNotPartOfZone of int
        | GetCloseTo of AgentName
        | Parried
        | Charged   of int option
        | Surveyed

    type Objective = 
        | Plan of (State -> (AgentAction list) option)
        | Requirement of Goal
        | MultiGoal of (State -> Goal list)

    type Intention = 
        {
            Label : string
            Type : IntentionType;
            Objectives : (Objective list);
            ChangeStateAfter : (State ->  State) option
            ChangeStateBefore : (State -> State) option
        }

    let statePrefix state = 
        sprintf "(%A): %A @ %A" state.SimulationStep state.Self.Name state.Self.Node

    let logStateCritical state = logPrefixCritical (statePrefix state)
    let logStateError state = logPrefixError (statePrefix state)
    let logStateImportant state = logPrefixImportant (statePrefix state)
    let logStateWarning state = logPrefixWarning (statePrefix state)
    let logStateInfo state = logPrefixInfo (statePrefix state)



