namespace NabfAgentLogic
module ActionSpecifications =

    open NabfAgentLogic.AgentTypes
    open Graphing.Graph
    open Constants
    open Logging

    type ConditionResult = 
        | Success
        | Failure of string

    type Condition = (State -> ConditionResult)

    let condToBool condres =
        match condres with
        | Success -> true
        | Failure _ -> false

    [<CustomEquality>]
    [<CustomComparison>]
    type ActionSpecification =
        { ActionType    : AgentAction
        ; Preconditions : Condition list 
        ; Effect        : State -> State
        ; Cost          : State -> int
        }
        override x.GetHashCode() = 0
        override self.Equals (other) = 
            match other with
            | :? ActionSpecification as spec -> spec.ActionType = self.ActionType
            | _ -> false

        interface System.IComparable with
            member self.CompareTo yobj =
                match yobj with
                | :? ActionSpecification as spec -> compare spec.ActionType self.ActionType
                | _ -> failwith "fsharp sucks"
    
    let isNotDisabled state = 
        match state.Self.Status = Normal with
        | true -> Success
        | false -> Failure "Agent is disabled"

    let enoughEnergy cost state = 
        match state.Self.Energy.Value >= cost with
        | true -> Success
        | false -> Failure <| sprintf "Not enough energy (cost: %A, have: %A)" cost state.Self.Energy.Value

    let deductEnergy cost state =
        { state.Self with Energy = Some <| state.Self.Energy.Value - cost }

    let definiteCost cost = 
        match cost with 
        | Some c -> c
        | None -> Constants.UNKNOWN_EDGE_COST

    let decide defaultValue opt =
        match opt with
        | Some ret -> ret
        | None -> defaultValue

    let findAgentPosition (agent : AgentName) agentList =
        let findAgent = List.find (fun a -> a.Name = agent) agentList
        findAgent.Node

    let inRangeOfAgent state agent fromList = 
        match state.Self.Node = findAgentPosition agent fromList with
        | true -> Success
        | false -> Failure <| sprintf "Not in range of agent %A" agent

    let rec tryRemoveFromList selector list =
        match list with
        | head :: tail when selector head -> Some (head, tail)  
        | head :: tail -> match (tryRemoveFromList selector tail) with
                          | Some (h, l) -> Some (h, head :: tail)
                          | None -> None
        | [] -> None

    let removeFromList selector list =
        match tryRemoveFromList selector list with
        | Some result -> result
        | None -> failwith "removeFromList: Element not found"

    let communicationAction action =
        { ActionType = Communicate action
        ; Preconditions = []
        ; Effect = fun state -> state
        ; Cost = fun _ -> 0
        }

    let moveAction (destination : VertexName) = 
        let edgeCost state = 
            //logInfo <| sprintf "At: %A, neighbours: %A, destination: %A" state.Self.Node state.World.[state.Self.Node].Edges destination
            let neighbour = 
                state.World.[state.Self.Node].Edges 
                |> Set.toList 
                |> List.tryFind (fun (_, name) -> name = destination)

            match neighbour with
            | Some (cost, _) -> Some cost
            | None -> None
        

        let cost state = decide Constants.ACTION_COST_CHEAP (edgeCost state).Value

        let updateState state = 
            let self = { state.Self with Node = destination }
            let newSelf = deductEnergy (cost state) { state with Self = self}
            //logImportant (sprintf "%A" (Set.filter (fun (o,_) -> Option.isSome o) state.World.[destination].Edges))
            let exploredNodes = if  ( Set.forall (fun (value, _) -> value = Option.None) state.World.[destination].Edges ) then 1 else 0
            { state with Self = newSelf; MyExploredCount = state.MyExploredCount + exploredNodes; LastAction = Action.Goto destination}

        let canMoveTo state = 
            match edgeCost state with
            | Some cost ->
                if (state.Self.Energy.Value - definiteCost cost) >= 0 then
                    Success
                else 
                    Failure "Not enough energy"
            | None -> Failure "Not a neighbour"

        { ActionType    = Perform <| Goto destination
        ; Preconditions = [ canMoveTo; isNotDisabled ]
        ; Effect        = updateState
        ; Cost          = cost
        }

    let attackAction (enemyAgent : AgentName) =
        let canAttack state = inRangeOfAgent state enemyAgent state.EnemyData

        let updateState state =
            let attacked, rest = List.partition (fun e -> e.Name = enemyAgent) state.EnemyData 
            let updateAtt = { List.head attacked with Status = Disabled }
            { state with EnemyData = updateAtt::rest; 
                         Self = deductEnergy Constants.ACTION_COST_EXPENSIVE state
                         LastAction = Action.Attack enemyAgent}
        
        { ActionType    = Perform <| Attack enemyAgent
        ; Preconditions = [ canAttack; enoughEnergy Constants.ACTION_COST_EXPENSIVE; isNotDisabled ]
        ; Effect        = updateState
        ; Cost          = fun _ -> Constants.ACTION_COST_EXPENSIVE
        }

    let rechargeAction =
        let updateState state = 
            let newEnergy = state.Self.Energy.Value + (int ((float state.Self.MaxEnergy.Value) * RECHARGE_FACTOR)) 
            { state with Self = { state.Self with Energy = Some newEnergy}; LastAction = Recharge }
        { ActionType    = Perform <| Recharge
        ; Preconditions = [  ]
        ; Effect        = updateState
        ; Cost = fun _ -> 1
        }       

    let repairAction (damagedAgent : AgentName) =
        let canRepair state = inRangeOfAgent state damagedAgent state.FriendlyData
        
        let repairCost state = 
            match state.Self.Status with
            | Normal   -> Constants.ACTION_COST_EXPENSIVE
            | Disabled -> Constants.ACTION_COST_DISABLED

        let updateState state =
            let repairedAgent, rest = removeFromList (fun a -> a.Name = damagedAgent) state.FriendlyData 
            let updateAgent = { repairedAgent with Health = repairedAgent.MaxHealth; Status = Normal }
            { state with FriendlyData = updateAgent :: rest;
                         Self = deductEnergy (repairCost state) state 
                         LastAction = Action.Repair damagedAgent
            }

        { ActionType    = Perform <| Repair damagedAgent
        ; Preconditions = [ canRepair; fun state -> enoughEnergy (repairCost state) state ]
        ; Effect        = updateState
        ; Cost          = fun state -> repairCost state
        }

    let probeAction vertexOption =
        let realVertex state = 
            match vertexOption with
            | Some vertex -> vertex
            | None -> state.Self.Node
        
        let vertexUnProbed state = 
            match state.World.[realVertex state].Value with
            | None -> Success
            | Some _ -> Failure <| sprintf "Vertex %A is already probed" (realVertex state)

        let updateState state = 
                                let vertex = (realVertex state)
                                let newWorld = addVertexValue vertex 0 state.World
                                { state with 
                                        World = newWorld
                                        Self = deductEnergy Constants.ACTION_COST_CHEAP state
                                        Probed = Set.add vertex state.Probed
                                        LastAction = Action.Probe vertexOption
                                }

        { ActionType    = Perform <| Probe vertexOption
        ; Preconditions = [ vertexUnProbed; enoughEnergy Constants.ACTION_COST_CHEAP; isNotDisabled ]
        ; Effect        = updateState
        ; Cost          = fun _ -> Constants.ACTION_COST_CHEAP
        }

    let inspectAction agentNameOption =
        let agentNames (state : State) = 
            match agentNameOption with
            | Some agentName -> [ agentName ]
            | None -> List.filter (fun enemy -> enemy.Node = state.Self.Node) state.EnemyData |> List.map (fun enemy -> enemy.Name)

        let enemiesNotInspected state = 
            let res = not <| List.forall (fun enemy -> Set.contains enemy state.InspectedEnemies) (agentNames state)
            match res with
            | true -> Success
            | false -> Failure <| sprintf "No uninspected agents present"

        let updateState (state : State) = 
            { state with 
                    InspectedEnemies = Set.union state.InspectedEnemies (agentNames state |> Set.ofList);
                    Self = deductEnergy Constants.ACTION_COST_EXPENSIVE state
                    LastAction = Action.Inspect agentNameOption
            }

        { ActionType    = Perform <| Inspect agentNameOption
        ; Preconditions = [ enemiesNotInspected; enoughEnergy Constants.ACTION_COST_EXPENSIVE; isNotDisabled ]
        ; Effect        = updateState
        ; Cost          = fun _ -> Constants.ACTION_COST_EXPENSIVE
        }

    let parryAction =
        
        let updateState state =
            { state with Self = deductEnergy Constants.ACTION_COST_EXPENSIVE state }

        let saboteurPresent state = 
            let res = List.exists (fun enemy -> enemy.Node = state.Self.Node && (enemy.Role = None || enemy.Role = Some Saboteur)) state.EnemyData
            match res with 
            | true -> Success
            | false -> Failure "No saboteur present"

        { ActionType    = Perform <| Parry
        ; Preconditions = [ saboteurPresent; enoughEnergy Constants.ACTION_COST_EXPENSIVE; isNotDisabled ]
        ; Effect        = updateState
        ; Cost          = fun _ -> Constants.ACTION_COST_EXPENSIVE
        }

    let unSatisfiedPreconditions state actionSpec =
        let failmsg = function
        | Failure msg -> Some msg
        | Success -> None

        List.choose failmsg <| List.map (fun prec -> prec state) actionSpec.Preconditions

    let isApplicable state actionSpec = 
        List.isEmpty <| unSatisfiedPreconditions state actionSpec

    let agentsAt node agentList =
        List.filter (fun enemy -> enemy.Node = node) agentList
        |> List.map (fun enemy -> enemy.Name)

    let gotoActions (state : State) = 
        List.map moveAction <| getNeighbourIds state.Self.Node state.World
    
    let attackActions (state : State) = 
        List.map attackAction <| agentsAt state.Self.Node state.EnemyData

    let rechargeActions state = [rechargeAction]

    let repairActions (state : State) = 
        List.map repairAction <| agentsAt state.Self.Node state.FriendlyData

    let probeActions state = [probeAction None]

    let inspectActions state = [inspectAction None]

    let parryActions state = [parryAction]

    let commonActions state = gotoActions state @ rechargeActions state

    let roleActions state =
        match state.Self.Role with
        | Some Explorer  -> probeActions state   @ commonActions state 
        | Some Inspector -> inspectActions state @ commonActions state 
        | Some Repairer  -> repairActions state  @ commonActions state 
        | Some Saboteur  -> attackActions state  @ commonActions state 
        | Some Sentinel  -> parryActions state   @ commonActions state 
        | None -> failwith "agent role is unknown"

    let actionSpecification (action : AgentAction) =
        match action with
        | Communicate comm -> communicationAction comm
        | _ -> raise (System.NotImplementedException ())

