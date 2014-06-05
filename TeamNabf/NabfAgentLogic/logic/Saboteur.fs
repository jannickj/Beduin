namespace NabfAgentLogic
module Saboteur =

    open FsPlanning.Agent.Planning
    open AgentTypes
    open LogicLib
    open Constants

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
        

        let (distanceToJob,personalValueMod) = (getDistanceToJobAndNumberOfEnemyNodes jobTargetNode s)
        
        //final desire
        int <| (((float newValue) * personalValueMod) - (float oldJobValue))    +     (-(distanceToJob * DISTANCE_TO_ATTACK_JOB_MOD))    +    SABOTEUR_ATTACKJOB_MOD


    let nodeHasEnemyAgent (state:State) node =
        List.exists (fun a -> a.Node = node && a.Status = EntityStatus.Normal) state.EnemyData

    ////////////////////////////////////////Logic////////////////////////////////////////////

    let applyToAttackJob (inputState:State) = 
        let applicationList = createApplicationList inputState JobType.AttackJob calculateDesireAttackJob
        Some(
                "apply to all attack jobs"
                , Communication
                , [Plan(fun state -> Some applicationList)]
            )

    let spontanouslyAttackAgentOnMyNode (inputState:State) = 
        let enemiesNearby = List.filter (fun a -> a.Node = inputState.Self.Node) inputState.EnemyData
        match enemiesNearby with
        | [] -> None
        | head::tail ->     
            Some(
                    "attack agent " + head.Name
                    , Activity
                    , [Requirement(
                                        ((fun state -> agentHasFulfilledRequirementEnemies head.Name state (fun ag -> ag.Status = EntityStatus.Disabled)), None, RepairGoal <| Some head.Name) 
                                    )]
                )
    
    let workOnAttackJob (inputState:State) = 
        let myJobs = List.map (fun (id,_) -> getJobFromJobID inputState id) inputState.MyJobs
        let myAttackJobs = getJobsByType JobType.AttackJob myJobs
        match myAttackJobs with
        | ((id,_,_,_),_)::_ -> 
            let (_,node) = List.find (fun (jid,_) -> id.Value = jid) inputState.MyJobs
            Some
                (   "attack agent on node " + node
                ,   Activity
                ,   [
                        Requirement <| ((fun state -> state.Self.Node = node), Some (fun state -> (distanceBetweenNodes state.Self.Node node state)), GotoGoal)
                    ;   Requirement <| ((fun state ->  
                                            match state.LastAction with
                                            | (Attack _) -> true
                                            | _ -> false
                                    ), None, AttackGoal None)
                    ]
                )
        | [] -> None
    
    let spontanouslyAttackAgent (inputState:State) = 
        let enemiesNearby = List.filter (fun a -> a.Status <> Disabled) (nearbyEnemies inputState inputState.Self)
        match enemiesNearby with
        | [] -> None
        | head::tail ->     
            Some(
                    "attack agent " + head.Name
                    , Activity
                    , [Requirement(
                                    ((fun state -> agentHasFulfilledRequirementEnemies head.Name state (fun ag -> ag.Status = EntityStatus.Disabled)), None, AttackGoal <| Some head.Name)
                    
                    )]
                )
             
    
    let applyToDisruptJob (inputState:State) = None //advanced feature
    
    let workOnDisruptJobThenParryIfEnemiesClose (inputState:State) = None //advanced feature
    
    let findAgentToDestroy (inputState:State) = 
        let worldArray = Map.toArray inputState.World
        let rand = System.Random()
        let index = rand.Next(0,inputState.World.Count)
        let target = worldArray.[index]
        Some
            (   "go to node " + (fst <| target)
            ,   Activity
            ,   [
                    //Requirement <| ((fun state -> (state.Self.Node = (fst <| target))), None )//Some (distanceBetweenAgentAndNode (fst <| target))
                    Plan <| planRouteTo (fst target)
                ]
            )
        //findAndDo inputState.Self.Node nodeHasEnemyAgent "attack an agent" false inputState
//        Some(
//                "find and destroy an agent"
//                , Activity
//                , [Requirement(
//                    fun state ->  
//                        match state.LastAction with
//                        | (Attack _) -> true
//                        | _ -> false
//                )]
//            )
