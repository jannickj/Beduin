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
    open IiLang.IilTranslator
    open IiLang.IiLangDefinitions
    open AgentClientLib

    type public AgentLogicClient(name) as this = class 
        
        
        [<DefaultValue>] val mutable private agent : BDIAgentImpl
        
        let MarsCom = new MarsCommunicator()
        let mutable simID = -1
                     
        let SendAgentServerEvent = new Event<UnaryValueHandler<IilAction>, UnaryValueEvent<IilAction>>()
        let SendMarsServerEvent = new Event<UnaryValueHandler<IilAction>, UnaryValueEvent<IilAction>>()
        let EvaluationCompletedEvent = new Event<EventHandler, EventArgs>()
        let EvaluationStartedEvent = new Event<EventHandler, EventArgs>()
        let SimulationEndedEvent = new Event<EventHandler, EventArgs>()
        
        do
            MarsCom.NewAction.Add(fun evt ->
                let id,act = evt.Value 
                let iilContainer =  buildIilActionContainer act (float id)
                let iilAction = buildIilAction iilContainer
                SendMarsServerEvent.Trigger(this,new UnaryValueEvent<IilAction>(iilAction))
                ())
            ()
        member private this.protectedExecute (name, action, returnedOnError) =
                try
                    action()
                with 
                | :? ThreadAbortException as e -> raise e
                | e -> 
                    let s = sprintf "%A" e.StackTrace
                    logError (name + " crashed with: " + e.Message + "\n" + s)
                    returnedOnError()

        interface IAgentLogic with
            member this.SetGoal goal =
                ()

            member this.Close() = 
                ()

            member this.HandlePercepts(iilpercepts) = 
                let ServerMessage = this.protectedExecute ("Parsing percepts",(fun () -> Some (parseIilPercepts iilpercepts)),(fun () -> None))
                match ServerMessage with
                | Some (AgentServerMessage msg) ->
                    match msg with
                    | _ -> ()
                | Some (MarsServerMessage msg) ->
                    match msg with
                    | SimulationEnd _ -> ()                       
                    | SimulationStart sData ->
                        simID <- sData.SimId
                        let subscribeAction = buildIilSendMessage (simID, SimulationSubscribe)
                        SendAgentServerEvent.Trigger(this, new UnaryValueEvent<IilAction>(subscribeAction))
                        
                        let initState = buildInitState (name, sData)
                        //Add agent to desires
                        //this.agent = new BDIAgentImpl(initState, 
                        this.agent.AddAcuator(MarsCom)
                        this.agent.AddSensor(MarsCom)
                        ()
                    | msg -> 
                        MarsCom.SetMessage (msg)
                | None -> ()
                    
           
            

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