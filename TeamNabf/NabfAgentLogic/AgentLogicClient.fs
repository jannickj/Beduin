namespace NabfAgentLogic
    open NabfAgentLogic.AgentInterfaces;
    open JSLibrary.Data.GenericEvents;
    open JSLibrary.IiLang;
    open JSLibrary;
    open JSLibrary.IiLang.DataContainers;
    open System;
    open System.Threading;
    open System.Linq;
    open AgentTypes
    open Logging
    open System.Reflection
    open System.Diagnostics
    

    type public AgentLogicClient(name) = class 
        
                     
        let SendAgentServerEvent = new Event<UnaryValueHandler<IilAction>, UnaryValueEvent<IilAction>>()
        let SendMarsServerEvent = new Event<UnaryValueHandler<IilAction>, UnaryValueEvent<IilAction>>()
        let EvaluationCompletedEvent = new Event<EventHandler, EventArgs>()
        let EvaluationStartedEvent = new Event<EventHandler, EventArgs>()
        let SimulationEndedEvent = new Event<EventHandler, EventArgs>()
       
        interface IAgentLogic with
            member this.SetGoal goal =
                ()

            member this.Close() = 
                ()

            member this.HandlePercepts(iilpercepts) = 
               ()
           
            

            [<CLIEvent>]
            member this.SendAgentServerAction = SendAgentServerEvent.Publish
            [<CLIEvent>]
            member this.SendMarsServerAction = SendMarsServerEvent.Publish

            [<CLIEvent>]
            member this.EvaluationCompleted = EvaluationCompletedEvent.Publish
            [<CLIEvent>]
            member this.EvaluationStarted = EvaluationStartedEvent.Publish
            [<CLIEvent>]
            member this.SimulationEnded  = SimulationEndedEvent.Publish
    end