namespace NabfAgentLogic
module MessageType =
    
    open Graphing.Graph
    open AgentTypes

    type RecipientName = AgentName
    type SenderName = AgentName

    type Message =
        | MyLocation of VertexName
        //add new types here

    type MailTo = RecipientName*Message
    type MailFrom = SenderName*Message



         

