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
    open Planning

    type public AgentLogicClient(name) as this = class 
        
        
        [<DefaultValue>] val mutable private agent : BDIAgentImpl
        
        let MarsCom = new MarsCommunicator()
        let MasterCom = new MasterCommunicator()
        let mutable simID = -1
                     
        let SendAgentServerEvent = new Event<UnaryValueHandler<IilAction>, UnaryValueEvent<IilAction>>()
        let SendMarsServerEvent = new Event<UnaryValueHandler<IilAction>, UnaryValueEvent<IilAction>>()
        let EvaluationCompletedEvent = new Event<EventHandler, EventArgs>()
        let EvaluationStartedEvent = new Event<EventHandler, EventArgs>()
        let SimulationEndedEvent = new Event<EventHandler, EventArgs>()
        
        let sendMasterServerMessage act =
            let iilContainer = buildIilMetaAction act simID
            let iilAction = buildIilAction iilContainer
            SendAgentServerEvent.Trigger(this,new UnaryValueEvent<IilAction>(iilAction))

        do
            MarsCom.NewAction.Add(fun evt ->
                let id,act = evt.Value 
                let iilContainer = buildIilActionContainer act (float id)
                let iilAction = buildIilAction iilContainer
                SendMarsServerEvent.Trigger(this,new UnaryValueEvent<IilAction>(iilAction))
                ())
            MasterCom.NewAction.Add(fun evt ->
                let act = evt.Value
                sendMasterServerMessage act
                )
            ()
        member private this.protectedExecute (name, action, returnedOnError) =
                try
                    action()
                with 
                | :? ThreadAbortException as e -> raise e
                | e -> 
                    let s = sprintf "%A" e.StackTrace
                    logError Agent (name + " crashed with: " + e.Message + "\n" + s)
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
                    | _ ->  
                            this.protectedExecute ("Reading Master Server message", (fun () -> MasterCom.SetMessage(msg)),(fun () -> ()))
                            ()
                | Some (MarsServerMessage msg) ->
                    match msg with
                    | SimulationEnd _ -> ()                       
                    | SimulationStart sData ->
                        simID <- sData.SimId
                        let subscribeAction = buildIilSendMessage (simID, SimulationSubscribe)
                        SendAgentServerEvent.Trigger(this, new UnaryValueEvent<IilAction>(subscribeAction))
                        
                        let initState = buildInitState (name, sData)
                        let desireTree = DesireTree.getTree
                        let planner = new AgentPlanner()
                        this.agent <- new BDIAgentImpl(initState, desireTree, planner)
                        this.agent.AddAcuator(MarsCom)
                        this.agent.AddSensor(MarsCom)
                        this.agent.AddAcuator(MasterCom)
                        this.agent.AddSensor(MasterCom)
                        ()
                    | msg ->
                        match msg with
                        | ActionRequest ((_,_,id),_) -> 
                            sendMasterServerMessage  <| NewRound id
                        | _ -> ()
                        this.protectedExecute ("Reading Mars Server message", (fun () ->  MarsCom.SetMessage (msg)),(fun () -> ()))
                       
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