namespace NabfAgentLogic
module Saboteur =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants
    open Graphing.Graph
    open GeneralLib

    ///////////////////////////////////Helper functions//////////////////////////////////////
    let calculateDesireAttackJob (j:Job) (s:State) = 
        let ((_,newValue,_,_),(jobData)) = j      
        let oldJobValue = 
                            if (s.MyJobs.IsEmpty) then
                                0
                            else
                                (getJobValueFromJoblist s.MyJobs s)

        let jobTargetNode = 
            match jobData with
            | AttackJob (zone) -> zone.Head
            | _ -> "None"
        
        if jobTargetNode = "None" then
            -1
        else

            let distanceToJob = (distanceBetweenAgentAndNode jobTargetNode s)
        
            let personalValueMod = 1 |> float//if an agent has some kind of "personal" preference 
                                                //that modifies how much it desires the new job, using the input modifier 
        
             
            let isEnabled =
                if (s.Self.Status = EntityStatus.Disabled) then
                    0.0
                else
                    1.0

            //final desire
            int <| (( JOB_IMPORTANCE_MODIFIER_ATTACK*(((float newValue) * personalValueMod) - (float oldJobValue))   +    (-((float distanceToJob) * DISTANCE_TO_ATTACK_JOB_MOD))) * isEnabled)


    ////////////////////////////////////////Logic////////////////////////////////////////////

    let applyToAttackJob (inputState:State) = 
        let applicationList = createApplicationList inputState JobType.AttackJob calculateDesireAttackJob
        Some <| normalIntention (
                "apply to all attack jobs"
                , Communication
                , [Plan(fun state -> Some applicationList)]
            )

    let spontanouslyAttackAgentOnMyNode (inputState:State) = 
        let ableEnemiesNearby = List.filter (fun a -> a.Node = inputState.Self.Node && a.Status = Normal) inputState.EnemyData
        match ableEnemiesNearby with
        | [] -> None
        | head::tail ->     
            Some <| normalIntention (
                    "attack agent " + head.Name
                    , Activity
                    , [Requirement <| Attacked head.Name]
                )
    
    let workOnAttackJob (inputState:State) = 
        let myJobs = List.map (fun (id,_) -> getJobFromJobID inputState id) inputState.MyJobs
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
                    "attack agent " + head.Name
                    , Activity
                    , [Requirement (Attacked head.Name)] 
                )
             
    
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
            Some <| normalIntention ( "go to node " + target, Activity, [ Requirement <| At target ] )
        else
            let index = rand.Next(0, List.length explored)
            let target = List.nth explored index
            Some <| normalIntention ( "go to node " + target, Activity, [ Requirement <| At target ] )
        

