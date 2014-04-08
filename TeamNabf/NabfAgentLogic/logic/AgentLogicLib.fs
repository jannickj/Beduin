namespace NabfAgentLogic
module AgentLogicLib =

    open Graphing.Graph
    open AgentTypes
    open PathFinding
    open Logging

    let ProbeCost = 1
    let SurveyCost = 1
    let InspectCost = 2
    let AttackCost = 2
    let ParryCost = 2
    let RepairCost = 2
    let RepairCostDisabled = 3
    let BuyCost = 2

    let desirabilityForDecreasingValue value = (1000000/(value+1))

    let adjacentAgents state = List.partition (fun agent -> agent.Team <> state.Self.Team) state.NearbyAgents
    let adjacentEnemies state = fst <| adjacentAgents state
    let adjacentFriends state = snd <| adjacentAgents state

    let desireFromPath agent graph node increaser = 
        let inc = if increaser < 0 then 0 else increaser
        let p = pathTo agent node graph
        match p with
        | Some road -> ((desirabilityForDecreasingValue ((List.length road) + inc)),true)
        | None -> (0,false)

    let recharge = (true,Some(Recharge))
     
    let tryDo (action : Action) (s:State) =
        match action with
        | Probe(o)  -> if s.Self.Energy.Value >= ProbeCost then (true,Some(Probe(o))) else  recharge
        | Survey    -> if s.Self.Energy.Value >= SurveyCost then (true,Some(Survey)) else recharge
        | Inspect(a)-> if s.Self.Energy.Value >= InspectCost then (true,Some(Inspect(a))) else  recharge
        | Attack(a) -> if s.Self.Energy.Value >= AttackCost then (true,Some(Attack(a))) else recharge
        | Parry     -> if s.Self.Energy.Value >= ParryCost then (true,Some(Parry)) else recharge
        | Repair(a) -> if ((s.Self.Energy.Value >= RepairCost) && (s.Self.Health.Value > 0)) || (s.Self.Energy.Value >= RepairCostDisabled) then (true,Some(Repair(a))) else recharge
        | Buy(u)    -> if s.Self.Energy.Value > BuyCost then (true,Some(Buy(u))) else recharge
        | _         -> printfn "PHILIP'S FAIL STORE.COM"; (false,None)

    

    //Only meant for moving to adjacent nodes
    let tryGo (v:Vertex) (s:State) =
        let edges = Set.toList s.World.[s.Self.Node].Edges
        let edge = List.find (fun (_,id) -> id = v.Identifier) edges

        if (fst edge) = None then 
            if s.Self.Energy < (Some 10) then 
                recharge 
            else 
                
                (true,Some(Goto(v.Identifier)))
        else
            let value = (fst edge).Value
            if value > s.Self.Energy.Value then 
     
                recharge 
            else 
        
                (true,Some(Goto(v.Identifier)))

    let pathingTryGo (vn:VertexName) (s:State) =
        let path = pathTo s.Self vn s.World
        if path.IsSome && not path.Value.IsEmpty then
            tryGo (s.World.[path.Value.Head]) s
        else
            (false,None)

    let rec getSafeVertex (s:State) (l:Vertex list) (enemySabs:Agent list)=
        match l with
        | [] -> None
        | head :: tail -> 
            if not (List.tryFind (fun (enemy:Agent) -> s.World.[enemy.Node] = head) enemySabs = None) 
            then 
                getSafeVertex s tail enemySabs
            else
                Some head

    let runAway (s:State) (enemySabs:Agent list)= 
        let neighbours = getNeighbours s.Self.Node s.World
       
        let partition = List.partition (fun (v:Vertex) ->   v.Identifier = s.LastPosition   ||     
                                                            not <| Set.forall (fun (cost,_) -> cost = None) v.Edges) neighbours
        let sortedNeighbours = List.append (snd partition) (fst partition)
        let v = getSafeVertex s sortedNeighbours enemySabs
        if not (v = None) 
        then 
            tryGo v.Value s
        else
            tryGo sortedNeighbours.Head s

    let rankByType (s:State) =
        let sameRankAllies = List.filter (fun a -> (a.Node = s.Self.Node) && (a.Team = s.Self.Team)) s.NearbyAgents
        let order = List.sort <| List.map (fun a -> a.Name) (s.Self :: sameRankAllies)
        (List.findIndex ((=) s.Self.Name) order)
