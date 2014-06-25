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
        let applicationList = createApplicationList inputState JobType.AttackJob calculateDesireAttackJob
        Some <| normalIntention (
                "apply to all attack jobs"
                , Communication
                , [Plan(fun state -> Some applicationList)]
            )

    let spontanouslyAttackAgentOnMyNode (inputState:State) = 
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
        let myJobs = List.map (fun (id,_) -> getJobFromJobID inputState.Jobs id) inputState.MyJobs
        let myAttackJobs = getJobsByType JobType.AttackJob myJobs
        match myAttackJobs with
        | ((Some id,_,_,_),_)::_ -> 
            let (_,node) = List.find (fun (jid,_) -> id = jid) inputState.MyJobs
            Some <| normalIntention 
                (   "attack agent on node " + node
                ,   Activity
                ,   [ Requirement <| At node 
                    ; Plan (fun state -> Some [Communicate (RemoveJob id)]) 
                    ]
                )
        | _ -> None
    
    let spontanouslyAttackAgent (inputState:State) = 
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
        let shouldAttack (agent:Agent) =
                agent.Status = Normal
             && agent.IsInVisionRange
             && (not (agent.Role = Some Sentinel && agent.RoleCertainty >= MINIMUM_ROLE_CERTAINTY))
        let healthyEnemies = List.filter shouldAttack inputState.EnemyData
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
        

