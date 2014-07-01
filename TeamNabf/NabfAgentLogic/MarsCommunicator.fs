namespace NabfAgentLogic
    open FsPlanning.Agent
    open System
    open AgentTypes
    open JSLibrary.Data.GenericEvents;

    open Logging
    

    type MarsCommunicator() =
        class
            let mutable requestedActId = -1
            let mutable awaitingPercepts = []

            let mutable actionSent = -1

            let perceptLock = new Object()
            let requestLock = new Object()

            let performActionLock = new Object()

            let NewPerceptsEvent = new Event<EventHandler, EventArgs>()
            let ActuatorReadyEvent = new Event<EventHandler, EventArgs>()
            let NewActionEvent = new Event<UnaryValueHandler<int*Action>, UnaryValueEvent<int*Action>>()

            let generateWaitForNewRound () = (Async.AwaitEvent ActuatorReadyEvent.Publish) |> Async.Ignore

            let rec sendAction actionSender action =
                lock performActionLock 
                    (fun () ->
                        let waitNewRound = generateWaitForNewRound()
                        let reqId = lock requestLock (fun () -> requestedActId)
                        if reqId <> -1 && actionSent <> reqId then
                            actionSender reqId action
                            actionSent <- reqId
                            actionSent
                        else
                            Async.RunSynchronously waitNewRound
                            sendAction actionSender action
                    )
            let rec waitForActionToFinish actId =
                let waitNewRound = generateWaitForNewRound()
                let reqid = lock requestLock (fun () -> requestedActId)
                if reqid > actId then
                    logInfo Agent <| sprintf "Waited for reqid %A to change" reqid
                    ()
                else
                    logInfo Agent <| sprintf "At reqid %A waiting for it to change" reqid
                    Async.RunSynchronously waitNewRound
                    waitForActionToFinish actId

            let sendActionAndAwaitFinish actionSender action =
                let sentId = sendAction actionSender action
                logInfo Agent <| sprintf "Sent action %A for id: %A" action sentId
                waitForActionToFinish sentId

            [<CLIEvent>]
            member this.NewAction = NewActionEvent.Publish

            member private this.ActionSender =
                (fun id act  -> NewActionEvent.Trigger(this, new UnaryValueEvent<_>((id,act))))

            member this.SetMessage (msg:MarsServerMessage) =
                match msg with
                | ActionRequest ((deadline,curtime,id),percepts) -> 
                    lock requestLock (
                        fun () -> 
                            requestedActId <- id
                            lock perceptLock (fun () -> awaitingPercepts <- awaitingPercepts@(NewRoundPercept::percepts)) 
                            NewPerceptsEvent.Trigger(this, new EventArgs())
                        )
                    ActuatorReadyEvent.Trigger(this, new EventArgs())                 
                | _ -> ()



            interface Actuator<AgentAction> with
                member this.CanPerformAction action =
                    match action with
                    | Perform _ -> true
                    | Communicate _ -> false
                
                member this.PerformActionBlockUntilFinished action =
                    match action with
                    | Perform act ->
                        sendActionAndAwaitFinish (this.ActionSender) act
                    | _ -> ()

                member this.PerformAction action =
                    match action with
                    | Perform act ->
                        ignore <| sendAction (this.ActionSender) act
                    | _ -> ()

                member this.IsReady =  lock requestLock (fun () -> actionSent <> requestedActId)

                [<CLIEvent>]
                member this.ActuatorReady = ActuatorReadyEvent.Publish

            interface Sensor<Percept> with
                member this.ReadPercepts() = 
                    lock perceptLock (fun () ->
                        let percepts = awaitingPercepts
                        awaitingPercepts <- []
                        percepts)
                [<CLIEvent>]
                member this.NewPercepts = NewPerceptsEvent.Publish

        end

