namespace NabfAgentLogic
module ActionSpecifications =

    open NabfAgentLogic.AgentTypes
    open Graphing.Graph
    open Constants

    type ActionSpecification =
        { ActionType    : Action
        ; Preconditions : (State -> bool) list
        ; Postcondition : (State -> State)        
        } 
    
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

    let moveAction (destination : VertexName) = 
        let edgeCost state = 
            state.World.[state.Self.Node].Edges 
            |> Set.toList 
            |> List.find (fun (cost, name) -> name = destination) 
            |> fst

        let updateState state = 
            let self = { state.Self with Node = destination }
            let newSelf = deductEnergy (decide Constants.ACTION_COST_CHEAP (edgeCost state)) { state with Self = self}
            { state with Self = newSelf}

        let canMoveTo state = (state.Self.Energy.Value - definiteCost (edgeCost state)) >= 0
        { ActionType    = Goto destination
        ; Preconditions = [ canMoveTo; isNotDisabled ]
        ; Postcondition = updateState
        }

    let attackAction (enemyAgent : AgentName) =
        let canAttack state = 
            state.Self.Node = findAgentPosition enemyAgent state.EnemyData

        let updateState state =
            let attacked, rest = List.partition (fun e -> e.Name = enemyAgent) state.EnemyData 
            let updateAtt = { List.head attacked with Status = Disabled }
            { state with EnemyData = updateAtt::rest; Self = deductEnergy Constants.ACTION_COST_EXPENSIVE state}
        
        { ActionType    = Attack enemyAgent
        ; Preconditions = [ canAttack; enoughEnergy Constants.ACTION_COST_EXPENSIVE; isNotDisabled ]
        ; Postcondition = updateState
        }

    let rechargeAction =
        let updateState state = 
            let newEnergy = state.Self.Energy.Value + (int ((float state.Self.MaxEnergy.Value) * RECHARGE_FACTOR)) 
            { state with Self = { state.Self with Energy = Some newEnergy} }
        { ActionType    = Recharge
        ; Preconditions = [  ]
        ; Postcondition = updateState
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

        { ActionType    = Repair damagedAgent
        ; Preconditions = [ canRepair; fun state -> enoughEnergy (repairCost state) state ]
        ; Postcondition = updateState
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
                                }

        { ActionType    = Probe vertexOption
        ; Preconditions = [ vertexUnProbed; enoughEnergy Constants.ACTION_COST_CHEAP; isNotDisabled ]
        ; Postcondition = updateState
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

        { ActionType    = Inspect agentNameOption
        ; Preconditions = [ enemiesNotInspected; enoughEnergy Constants.ACTION_COST_EXPENSIVE; isNotDisabled ]
        ; Postcondition = updateState
        }

    let parryAction =
        
        let updateState state =
            { state with Self = deductEnergy Constants.ACTION_COST_EXPENSIVE state }

        let saboteurPresent state = 
            List.exists (fun enemy -> enemy.Node = state.Self.Node && (enemy.Role = None || enemy.Role = Some Saboteur)) state.EnemyData

        { ActionType    = Parry
        ; Preconditions = [ saboteurPresent; enoughEnergy Constants.ACTION_COST_EXPENSIVE; isNotDisabled ]
        ; Postcondition = updateState
        }
