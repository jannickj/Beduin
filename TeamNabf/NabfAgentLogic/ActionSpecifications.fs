namespace NabfAgentLogic
module ActionSpecifications =

    open NabfAgentLogic.AgentTypes
    open Graphing.Graph
    open Constants
    open Logging

    [<CustomEquality>]
    [<CustomComparison>]
    type ActionSpecification =
        { ActionType    : AgentAction
        ; Preconditions : (State -> bool) list 
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
    
    let isNotDisabled state = state.Self.Status = Normal

    let enoughEnergy cost state = state.Self.Energy.Value >= cost

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
        ; Cost = 0
        }

    let moveAction (destination : VertexName) = 
        let edgeCost state = 
            state.World.[state.Self.Node].Edges 
            |> Set.toList 
            |> List.find (fun (cost, name) -> name = destination) 
            |> fst

        let cost state = decide Constants.ACTION_COST_CHEAP (edgeCost state)

        let updateState state = 
            let self = { state.Self with Node = destination }
            let newSelf = deductEnergy (cost state) { state with Self = self}
            logImportant (sprintf "%A" (Set.filter (fun (o,_) -> Option.isSome o) state.World.[destination].Edges))
            let exploredNodes = if  ( Set.forall (fun (value, _) -> value = Option.None) state.World.[destination].Edges ) then 1 else 0
            { state with Self = newSelf; MyExploredCount = state.MyExploredCount + exploredNodes}

        let canMoveTo state = (state.Self.Energy.Value - definiteCost (edgeCost state)) >= 0
        { ActionType    = Perform <| Goto destination
        ; Preconditions = [ canMoveTo; isNotDisabled ]
        ; Effect        = updateState
        ; Cost          = cost
        }

    let attackAction (enemyAgent : AgentName) =
        let canAttack state = 
            state.Self.Node = findAgentPosition enemyAgent state.EnemyData

        let updateState state =
            let attacked, rest = List.partition (fun e -> e.Name = enemyAgent) state.EnemyData 
            let updateAtt = { List.head attacked with Status = Disabled }
            { state with EnemyData = updateAtt::rest; Self = deductEnergy Constants.ACTION_COST_EXPENSIVE state}
        
        { ActionType    = Perform <| Attack enemyAgent
        ; Preconditions = [ canAttack; enoughEnergy Constants.ACTION_COST_EXPENSIVE; isNotDisabled ]
        ; Effect        = updateState
        ; Cost          = fun _ -> Constants.ACTION_COST_EXPENSIVE
        }

    let rechargeAction =
        let updateState state = 
            let newEnergy = state.Self.Energy.Value + (int ((float state.Self.MaxEnergy.Value) * RECHARGE_FACTOR)) 
            { state with Self = { state.Self with Energy = Some newEnergy} }
        { ActionType    = Perform <| Recharge
        ; Preconditions = [  ]
        ; Effect        = updateState
        ; Cost = fun _ -> 1
        }       

    let repairAction (damagedAgent : AgentName) =
        let canRepair state =
            state.Self.Node = findAgentPosition damagedAgent state.FriendlyData
        
        let repairCost state = 
            match state.Self.Status with
            | Normal   -> Constants.ACTION_COST_EXPENSIVE
            | Disabled -> Constants.ACTION_COST_DISABLED

        let updateState state =
            let repairedAgent, rest = removeFromList (fun a -> a.Name = damagedAgent) state.FriendlyData 
            let updateAgent = { repairedAgent with Health = repairedAgent.MaxHealth; Status = Normal }
            { state with FriendlyData = updateAgent :: rest;
                         Self = deductEnergy (repairCost state) state 
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
            Option.isSome state.World.[realVertex state].Value

        let updateState state = { state with 
                                        World = addVertexValue (realVertex state) 0 state.World;
                                        Self = deductEnergy Constants.ACTION_COST_CHEAP state
                                        MyProbedCount = state.MyProbedCount + 1
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
            not <| List.forall (fun enemy -> Set.contains enemy state.InspectedEnemies) (agentNames state)

        let updateState (state : State) = 
            { state with 
                    InspectedEnemies = Set.union state.InspectedEnemies (agentNames state |> Set.ofList);
                    Self = deductEnergy Constants.ACTION_COST_EXPENSIVE state
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
            List.exists (fun enemy -> enemy.Node = state.Self.Node && (enemy.Role = None || enemy.Role = Some Saboteur)) state.EnemyData

        { ActionType    = Perform <| Parry
        ; Preconditions = [ saboteurPresent; enoughEnergy Constants.ACTION_COST_EXPENSIVE; isNotDisabled ]
        ; Effect        = updateState
        ; Cost          = fun _ -> Constants.ACTION_COST_EXPENSIVE
        }

    let isApplicable state actionSpec = 
        List.forall (fun pred -> pred state) actionSpec.Preconditions

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

