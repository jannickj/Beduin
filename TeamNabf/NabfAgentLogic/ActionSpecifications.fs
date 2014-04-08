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

    let definiteCost cost = 
        match cost with 
        | Some c -> c
        | None -> Constants.UNKNOWN_EDGE_COST

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
        let canMoveTo state = (state.Self.Energy.Value - definiteCost (edgeCost state)) >= 0
        { ActionType    = Goto destination
        ; Preconditions = [ canMoveTo ]
        ; Postcondition = fun state -> { state with Self = { state.Self with Node = destination }}
        }

    let attackAction (enemyAgent : AgentName) =
        let canAttack state = 
            state.Self.Node = findAgentPosition enemyAgent state.EnemyData

        let updateState state =
            let attacked, rest = List.partition (fun e -> e.Name = enemyAgent) state.EnemyData 
            let updateAtt = { List.head attacked with Status = Disabled }
            { state with EnemyData = updateAtt::rest }
        
        { ActionType    = Attack enemyAgent
        ; Preconditions = [ canAttack ]
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

        let updateState state =
            let repairedAgent, rest = removeFromList (fun a -> a.Name = damagedAgent) state.FriendlyData 
            let updateAgent = { repairedAgent with Health = repairedAgent.MaxHealth; Status = Normal }
            { state with FriendlyData = updateAgent :: rest }

        { ActionType    = Repair damagedAgent
        ; Preconditions = [ canRepair ]
        ; Postcondition = updateState
        }
