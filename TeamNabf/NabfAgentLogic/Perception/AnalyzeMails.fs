namespace NabfAgentLogic.Perception
module AnalyzeMails =
    open NabfAgentLogic.AgentTypes
    open NabfAgentLogic
    open NabfAgentLogic.GeneralLib
    open PerceptionLib
    open Logging

    let updateStateWithMail (mail:Mail) (state:State) =
        let (sender,recipient,message) = mail
        match message with
        | GoingToRepairYou ->
            { state with Relations = Map.add MyRepairer sender state.Relations  }
        | MyLocation vn ->
            logStateImportant state Perception <| sprintf "Got Position %A of %A" vn sender 
            let updatedFriendlyList = updateAgentPosition sender vn state.Self.Team false state.FriendlyData
            { state with FriendlyData = updatedFriendlyList }        
