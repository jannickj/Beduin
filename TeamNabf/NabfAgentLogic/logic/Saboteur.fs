namespace NabfAgentLogic
module Saboteur =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants
    open Graphing.Graph
    open GeneralLib
    open Common

    ///////////////////////////////////Helper functions//////////////////////////////////////
    let calculateDesireAttackJob (j:Job) (s:State) = 
        let normalJobDesire = calculateJobDesire JOB_IMPORTANCE_MODIFIER_ATTACK DISTANCE_TO_ATTACK_JOB_MOD 0.0 j s
        let ((_,newValue,_,_),(jobData)) = j
        let timeStamp = 
                match jobData with
                | AttackJob (_,roundnumber) -> roundnumber
                | _ -> failwith "Attack job: %A does not have a round number" j

        let jobAge = float (s.SimulationStep - timeStamp)

        let ageDesire = (jobAge * VALUE_DECAY_PER_TURN)*JOB_AGE_VALUE_DECREASE_FACTOR

        normalJobDesire - (int ageDesire)
        

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let applyToAttackJob (inputState:State) = 
        if (inputState.Self.Status = EntityStatus.Disabled) then
            None
        else
            let applicationList = createApplicationList inputState JobType.AttackJob calculateDesireAttackJob
            Some <| normalIntention (
                    "apply to all attack jobs"
                    , Communication
                    , [Plan(fun state -> Some applicationList)]
                )

    let spontanouslyAttackAgentOnMyNode (inputState:State) = 
        if (inputState.Self.Status = EntityStatus.Disabled) then
            None
        else
            let shouldAttack (agent:Agent) =   
                   agent.Node = inputState.Self.Node 
                && agent.Status = Normal 
                && (not (agent.Role = Some Sentinel && agent.RoleCertainty >= MINIMUM_ROLE_CERTAINTY))

            let ableEnemiesNearby = List.filter shouldAttack inputState.EnemyData
            match ableEnemiesNearby with
            | [] -> None
            | head::tail ->     
                Some <| normalIntention (
                        "attack agent " + head.Name
                        , Activity
                        , [Plan (fun _ -> Some [Perform <| Attack head.Name])]
                    )
    
    let workOnAttackJob (inputState:State) = 
        if (inputState.Self.Status = EntityStatus.Disabled) then
            None
        else
            
            let myJobs = List.map (fun (id,_) -> getJobFromJobID inputState.Jobs id) inputState.MyJobs
            let myAttackJobs = getJobsByType JobType.AttackJob myJobs
            match myAttackJobs with
            | ((Some id,_,_,_),AttackJob(nodes,_))::_ -> 
                
                let occupyJobs = getJobsByType JobType.OccupyJob inputState.Jobs

                let defendJob (job:Job) =
                    match job with
                    | _,OccupyJob(_,zn) -> List.exists (fun n -> List.exists ((=) n) nodes) zn
                    | _ -> false
                
                let defendOccupyJobs = List.filter defendJob occupyJobs

                let occupyNodes = List.collect getOccupyZone defendOccupyJobs

                let allNodes = Set.union (Set nodes) (Set occupyNodes)
                
                let neighbours = List.collect ((flip getNeighbourIds) inputState.World) <| Set.toList allNodes

                let allNodesWithNeighbours =Set.toList <| Set.union (Set neighbours) (Set allNodes)

                let nodesToTargetForAttack = List.map (fun n -> Requirement <| KillAll n ) allNodesWithNeighbours

                Some <| normalIntention 
                    (   sprintf "attack agents on nodes: %A"  allNodesWithNeighbours
                    ,   Activity
                    ,   nodesToTargetForAttack @ [Plan (fun state -> Some [Communicate (RemoveJob id)])]
                    )
            | _ -> None
    
    let spontanouslyAttackAgent (inputState:State) = 
        if (inputState.Self.Status = EntityStatus.Disabled) then
            None
        else
            let enemiesNearby = List.filter (fun a -> a.Status <> Disabled) (nearbyEnemies inputState inputState.Self)
            match enemiesNearby with
            | [] -> None
            | head::tail ->     
                Some <| normalIntention (
                        "spontanously attack agent " + head.Name
                        , Activity
                        , [Requirement (Attacked head.Name)] 
                    )
             
    let killAgentICanSee (inputState:State) =
        if (inputState.Self.Status = EntityStatus.Disabled) then
            None
        else
            let visibleAgents = agentsInVision inputState.EnemyData
            let healthyEnemies = List.filter shouldAttack visibleAgents
            if List.length healthyEnemies > 0 then
                let closest = List.minBy (fun a -> distanceBetweenAgentAndNode a.Node inputState) healthyEnemies
                let killAgent = closest.Name
                Some <| 
                    normalIntention (
                        ("attack agent " + killAgent + " that i see")
                        , Activity
                        , [Requirement (Attacked killAgent)] 
                        )
            else
                None
    
    let patrolAZone (inputState:State) =
        let occupyJobs = getJobsByType JobType.OccupyJob inputState.Jobs
        if List.length occupyJobs > 0 then
            let rand = System.Random()
            let index = rand.Next(0, List.length occupyJobs)
            let target = List.nth occupyJobs index
            let goals = List.map (KillAll >> Requirement) <| getOccupyZone target
            normalIntention
                (   sprintf "patrol zone %A with nodes %A" (getJobId target) (getOccupyZone target)
                ,   Activity
                ,   goals
                )
            |> Some
        else
            None

    let applyToDisruptJob (inputState:State) = None //advanced feature
    
    let workOnDisruptJobThenParryIfEnemiesClose (inputState:State) = None //advanced feature
    
    let findAgentToDestroy (inputState:State) = 
        let neighbourIds = (getNeighbourIds inputState.Self.Node inputState.World)           
        let neighbours = 
                            if (neighbourIds.Length = 1) then
                                neighbourIds
                            else
                                List.filter ((<>) inputState.LastPosition) neighbourIds
        let (unExplored,explored) = List.partition (fun name -> isUnexplored inputState name) neighbours
        let rand = System.Random()
        if not unExplored.IsEmpty
        then
            let index = rand.Next(0, List.length unExplored)
            let target = List.nth unExplored index
            Some <| normalIntention ( "find agent to destroy by going to random node " + target, Activity, [ Requirement <| At target ] )
        else
            let index = rand.Next(0, List.length explored)
            let target = List.nth explored index
            Some <| normalIntention ( "find agent to destroy by going to random node " + target, Activity, [ Requirement <| At target ] )
        

