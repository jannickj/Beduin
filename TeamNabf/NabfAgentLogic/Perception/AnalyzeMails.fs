namespace NabfAgentLogic.Perception
module AnalyzeMails =
    open NabfAgentLogic.AgentTypes
    open NabfAgentLogic
    open NabfAgentLogic.GeneralLib
    open PerceptionLib

    let updateStateWithMail (mail:Mail) (state:State) =
        let (sender,recipient,message) = mail
        match message with
        | GoingToRepairYou ->
            { state with Relations = Map.add MyRepairer sender state.Relations  }
        | MyLocation vn ->
            let updatedFriendlyList = updateAgentPosition sender vn state.Self.Team false state.FriendlyData
            { state with FriendlyData = updatedFriendlyList }        
