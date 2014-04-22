namespace NabfAgentLogic
    open FsPlanning.Agent
    open System
    open AgentTypes
    open JSLibrary.Data.GenericEvents;


    

    type MarsCommunicator() =
        class
            let mutable curActId = -1
            let mutable awaitingPercepts = []

            let mutable actionSent = -1

            let perceptLock = new Object()
            let actionLock = new Object()

            let NewPerceptsEvent = new Event<EventHandler, EventArgs>()
            let ActuatorReadyEvent = new Event<EventHandler, EventArgs>()
            let NewActionEvent = new Event<UnaryValueHandler<int*Action>, UnaryValueEvent<int*Action>>()

            [<CLIEvent>]
            member this.NewAction = NewActionEvent.Publish

            member this.SetMessage (msg:MarsServerMessage) =
                match msg with
                | ActionRequest ((deadline,curtime,id),percepts) -> 
                    lock actionLock (fun () -> curActId <- id)
                    lock perceptLock (fun () -> awaitingPercepts <- awaitingPercepts@percepts) 
                    NewPerceptsEvent.Trigger(this, new EventArgs()) 
                    ActuatorReadyEvent.Trigger(this, new EventArgs())                 
                | _ -> ()


            interface Actuator<AgentAction> with
                member this.CanPerformAction action =
                    match action with
                    | Perform _ -> true
                    | Communicate _ -> false

                member this.PerformAction action =
                    match action with
                    | Perform act ->
                        let id = lock actionLock (fun () -> actionSent <- curActId
                                                            curActId)
                        NewActionEvent.Trigger(this, new UnaryValueEvent<_>((id,act)))
                    | _ -> ()
                member this.IsReady =  lock actionLock (fun () -> actionSent = curActId)

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

