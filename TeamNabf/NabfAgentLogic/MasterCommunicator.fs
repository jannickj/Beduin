namespace NabfAgentLogic
    open FsPlanning.Agent
    open System
    open AgentTypes
    open JSLibrary.Data.GenericEvents;

    type MasterCommunicator() =
        class
            
            let mutable awaitingPercepts = []


            let perceptLock = new Object()
            let actionLock = new Object()

            let NewPerceptsEvent = new Event<EventHandler, EventArgs>()
            let ActuatorReadyEvent = new Event<EventHandler, EventArgs>()
            let NewActionEvent = new Event<UnaryValueHandler<CommunicationAction>, UnaryValueEvent<CommunicationAction>>()

            [<CLIEvent>]
            member this.NewAction = NewActionEvent.Publish


            member this.SetMessage (msg:AgentServerMessage) =
                match msg with
                | JobMessage jobpercept -> 
                        lock perceptLock (fun () -> awaitingPercepts <- (JobPercept jobpercept)::awaitingPercepts)
                | SharedPercepts percepts ->
                        lock perceptLock (fun () -> awaitingPercepts <- percepts @ awaitingPercepts)
                | _ -> ()
                 
                NewPerceptsEvent.Trigger(this, new EventArgs())

            interface Actuator<AgentAction> with
                member this.CanPerformAction action =
                    match action with
                    | Perform _ -> false
                    | Communicate _ -> true

                member this.PerformAction action =
                    match action with
                    | Communicate act ->
                        match act with
                        | ShareKnowledge pl -> lock perceptLock (fun () -> awaitingPercepts <- (KnowledgeSent pl)::awaitingPercepts)
                        | _ -> ()
                        lock actionLock (fun () -> NewActionEvent.Trigger(this, new UnaryValueEvent<_>((act))))
                        ()
                    | _ -> ()
                member this.IsReady =  true

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