
namespace NabfAgentLogic
module AnalyzeBeliefs =
    
    open NabfAgentLogic.AgentTypes   
    open NabfAgentLogic.GeneralLib
    open NabfAgentLogic.LogicLib

    let updateStateBasedOnBelifs (state:State) =
        match state.Self.Role.Value with
//        | Explorer ->
//            //Assume nearby nodes will be probed by other agents
//            let isContender = predicateAnd (isSameRoleSameNode state.Self) isAlive 
//            let contenders =  state.Self :: List.filter isContender state.FriendlyData
//
//            let sortedContenders = List.sortBy getAgentName contenders
//            let nearbyNodes = getNClosestSatisfyingNodes state (isUnprobed state) (List.length sortedContenders)
//            let trimedContenders,trimedNearby = trimListToShortest sortedContenders nearbyNodes
//            match List.tryFindIndex (getAgentName >> ((=) state.Self.Name)) trimedContenders with
//            | Some idx -> 
//                let nodes = Seq.toList <| Seq.truncate idx trimedNearby
//                let setZeroValue (s:State) node = { s with World = Graphing.Graph.addVertexValue node 0 s.World }
//                List.fold setZeroValue state nodes
//            | _ -> state
        | _ -> state
