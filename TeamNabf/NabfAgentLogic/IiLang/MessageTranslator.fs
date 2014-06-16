namespace NabfAgentLogic.IiLang
module MessageTranslator =
    open NabfAgentLogic.AgentTypes

    let buildMail ((sender,recipient,msg):Mail) =
        let parseMsg = 
            match msg with
            | MyLocation vn -> "myloc "+vn

        (((sender+" "+parseMsg):string), recipient)
            
    let readMail (recipient:RecipientName) (text:string) =
        let chopped = List.ofArray <| text.Split [|' '|]
        match chopped with
        | sender::textmsg ->
            let mailmsg =
                match textmsg with
                | ["myloc"; vn] ->
                    MyLocation vn

                | _ -> failwith ("unknown mail message "+text)
            (sender,recipient,mailmsg):Mail
        | [] -> failwith ("mail was empty")

