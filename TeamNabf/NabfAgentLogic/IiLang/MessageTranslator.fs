namespace NabfAgentLogic.IiLang
module MessageTranslator =
    open NabfAgentLogic.MessageType

    let buildMail ((recipient,msg):MailTo) =
        let parseMsg = 
            match msg with
            | MyLocation vn -> "hej"
        parseMsg 
            
        

