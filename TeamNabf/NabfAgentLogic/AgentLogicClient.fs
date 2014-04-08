namespace NabfAgentLogic
    open NabfAgentLogic.AgentInterfaces;
    open JSLibrary.Data.GenericEvents;
    open JSLibrary.IiLang;
    open JSLibrary;
    open JSLibrary.IiLang.DataContainers;
    open System;
    open NabfAgentLogic.AgentLogic;
    open System.Threading;
    open System.Linq;
    open AgentTypes
    open Logging
    open System.Reflection
    open System.Diagnostics
    open DebuggingFeatures
    

    type public AgentLogicClient(name,decisionTree) = class 
        
        
        [<DefaultValue>] val mutable private BeliefData : State
        [<DefaultValue>] val mutable private KnownJobs : List<Job>

        [<DefaultValue>] val mutable private awaitingPercepts : List<Percept>
        [<DefaultValue>] val mutable private simulationID : SimulationID

        let agentname = name
        let decisionTree = decisionTree
        let mutable simEnded = false
        let mutable runningCalcID = 0
        let mutable runningCalc = 0
        let mutable decisionId = 0
        let mutable decidedAction = (Int32.MaxValue,Action.Skip)
 
        let mutable runningCalcNames = []
        
        let mutable recievedJobFromServer = false
        let mutable jobDecideCalcs  = 0

        
        
       
        new(name,debugmode:bool) = AgentLogicClient(name,debugModeTree)
        new(name) = AgentLogicClient(name,defaultDecisionTree)

        //Parallel helpers
        let mutable stopDeciders = new CancellationTokenSource()


        let stateLock = new Object()
        let decisionLock = new Object()
        let runningCalcLock = new Object()
        let knownJobsLock = new Object()
        let awaitingPerceptsLock = new Object()
        


        let SendAgentServerEvent = new Event<UnaryValueHandler<IilAction>, UnaryValueEvent<IilAction>>()
        let SendMarsServerEvent = new Event<UnaryValueHandler<IilAction>, UnaryValueEvent<IilAction>>()
        let EvaluationCompletedEvent = new Event<EventHandler, EventArgs>()
        let EvaluationStartedEvent = new Event<EventHandler, EventArgs>()
        let SimulationEndedEvent = new Event<EventHandler, EventArgs>()

        

        member private this.protectedExecute (name, action, returnedOnError) =
            try
                action()
            with 
            | :? ThreadAbortException as e -> raise e
            | e -> 
                let s = sprintf "%A" e.StackTrace
                logError (name + " crashed with: " + e.Message + "\n" + s)
                returnedOnError()

        
             

        member private this.protectedExecute (name, action) = this.protectedExecute (name, action, (fun () -> ()))

        member public this.DecidedAction = decidedAction

        member private this.asyncCalculation id name stopToken func = this.asyncCalculationAF id name stopToken  (async {  func() })
        
        let rec remove i l =
            match i, l with
            | 0, x::xs -> xs
            | i, x::xs -> x::remove (i - 1) xs
            | i, [] -> failwith "index out of range"
                                                                  
        member private this.asyncCalculationAF id name (stopToken:CancellationToken) func =
            //logInfo ("Async call: "+name)
            if stopToken.IsCancellationRequested then
                logError (name+" was cancelled before execution\n")
                        
            else
                let changeCals value = 
                    lock runningCalcLock (fun () -> 
                    if id = runningCalcID then
                        if runningCalc = 0 then
                            EvaluationStartedEvent.Trigger(this, new EventArgs())
                        runningCalc <- runningCalc + value
                        if value > 0 then
                            runningCalcNames <- name::runningCalcNames
                        else
                            runningCalcNames <- remove (List.findIndex (fun n -> n = name) runningCalcNames) runningCalcNames
                        
                        if runningCalc = 0 then
                            EvaluationCompletedEvent.Trigger(this, new EventArgs())
                    )
                let AsyncF f =
                    async
                        {
                            
                            //use! handler = Async.OnCancel(fun () -> ( logWarning (name+" was cancelled") ))
                            let action() =
                                changeCals(1)
                                logAll("Starting: "+name)
                                Async.StartImmediate(f,stopToken)
                                logAll ("Finished: "+name)
                                changeCals(-1)
                            this.protectedExecute (name, action)
                            
                            
                            
                        }
                

                Async.Start (AsyncF func,stopToken)
                () 

         member private this.asyncCalculationMany id name func values stopToken =
            List.iter (fun v -> ignore <| (this.asyncCalculationAF id name stopToken (async { ignore <| func v }))) values

        member private this.EvaluateDecision    (rankCur:DecisionRank) 
                                                (stopToken:CancellationToken) 
                                                (stopSource:CancellationTokenSource) 
                                                (s:State,d:Decision<(State -> (bool*Option<Action>))>) =
            match d with
            | Choice f -> 
                this.asyncCalculationAF runningCalcID ("Calc Choice "+f.ToString()) stopToken (
                    async
                        { 
                           
                            let dId = lock decisionLock (fun () -> decisionId)
                           
                            let cancelFunc() =
                                stopSource.Cancel()                            
                            use! handler = Async.OnCancel(cancelFunc)
                            let output = ref (false,Option.None)
                            let timer:float = 10.0
                            let success = Parallel.TryExecute<(bool*Option<Action>)>
                                                (   fun () -> this.protectedExecute ( f.ToString(), (fun () -> f s), (fun () -> (false,None)))
                                                ,   timer
                                                ,   (fun () -> stopToken.IsCancellationRequested)
                                                ,   output
                                                );
                            
                            
                            if success then
                                lock decisionLock (fun () ->
                                    if dId = decisionId then
                                        let (b,a) = output.Value
                                        let (cR,cA) = decidedAction
                                        logImportant (sprintf "(%A): %A: %A -> %A" s.Self.Name rankCur f b)
                                        if b && a.IsSome && cR > rankCur then
                                            logImportant (sprintf "(%A): Chosen: %A" s.Self.Name (f s))
                                            decidedAction <- (rankCur,a.Value)
                                            stopSource.Cancel() 
                                    else
                                        stopSource.Cancel()
                                    )
                            else
                                //logWarning (rankCur.ToString()+": "+f.ToString()+" was cancelled")
                                stopSource.Cancel() 
                        }
                    )
                rankCur + 1
            | Options ds -> 
                let (r,_) = List.fold ( fun (r,c) t ->
                                        let iteStopSource = new CancellationTokenSource()
                                        let nR = this.EvaluateDecision r c iteStopSource (s,t)
                                        (nR,iteStopSource.Token)
                                        ) (rankCur,stopToken) ds
                r
            | Condition (c,d) -> 
                let (b,_) = c s
                if b then
                    let source = new CancellationTokenSource()
                    this.EvaluateDecision (rankCur) stopSource.Token source (s,d)
                else
                    rankCur
//                this.asyncCalculationAF runningCalcID ("Calc conditon") stopToken (
//                            async
//                                {
//                                    use! handler = Async.OnCancel(fun () -> stopSource.Cancel())
//                                    
//                                })
                    




            

        member public this.EvaluteState() =
            lock decisionLock ( fun () ->
                                    decidedAction <- (Int32.MaxValue,Action.Skip)
                                )
            let s = lock stateLock (fun () -> this.BeliefData)
            ignore <| this.EvaluateDecision 0 stopDeciders.Token (new CancellationTokenSource()) (s,decisionTree)
            

       

        member private this.ReEvaluate percepts =
            stopDeciders.Cancel()
            stopDeciders <- new CancellationTokenSource()
            lock runningCalcLock (fun () -> 
                runningCalc <- 0
                runningCalcID <- runningCalcID + 1
                runningCalcNames <- []
                )
            recievedJobFromServer <- false
            let sharedPercepts = lock awaitingPerceptsLock (fun () -> 
                                        let percepts = this.awaitingPercepts
                                        this.awaitingPercepts <- []
                                        percepts)
            let oldstate = (lock stateLock (fun () -> this.BeliefData))
            let generateSharedPercepts() =
                let sharedP = (selectSharedPercepts oldstate percepts)
                if not ( List.isEmpty sharedP) then
                    let sharedPs = ShareKnowledge sharedP
                    let action = buildIilSendMessage (this.simulationID,sharedPs)
                    SendAgentServerEvent.Trigger(this, new UnaryValueEvent<IilAction>(action))
                else
                    ()
            this.asyncCalculation runningCalcID "generating shared percept" stopDeciders.Token generateSharedPercepts
            
            let newstate = lock stateLock (fun () -> 
                            let action() =
                                this.BeliefData <- updateState this.BeliefData (percepts@sharedPercepts) (lock knownJobsLock (fun () -> this.KnownJobs))
                                this.BeliefData
                            let onFail() =
                                lock awaitingPerceptsLock (fun () -> this.awaitingPercepts <- this.awaitingPercepts@sharedPercepts)
                                this.BeliefData
                            this.protectedExecute("State Update",action,onFail)
                            )

            let newRoundAct = buildIilSendMessage (this.simulationID, NewRound(this.BeliefData.SimulationStep))
            SendAgentServerEvent.Trigger(this, new UnaryValueEvent<IilAction>(newRoundAct))
            
            lock decisionLock ( fun () ->
                                    decisionId <- (decisionId + 1)
                                )
            
            this.EvaluteState()
            

        let stopLogic () =
            stopDeciders.Cancel()
            simEnded <- true
        
        member private this.CalculateAcceptedJob id moveTo =
            let foundJob = List.tryFind (fun ((jid,_,_,_),_) -> jid = id) this.KnownJobs
            if foundJob.IsSome then
                let update = 
                    async
                        {
                            
                            let ajob = foundJob.Value
                            lock stateLock (fun () ->   this.BeliefData <- updateStateWhenGivenJob this.BeliefData ajob moveTo)
                            //this.asyncCalculation runningCalcID "Calc accept job" stopDeciders.Token (fun () -> ignore <| this.EvaluteState())
                        }
                Async.StartImmediate update
            else
                lock stateLock (fun () ->this.BeliefData <- updateStateWhenLostJob this.BeliefData)
                //this.asyncCalculation runningCalcID "Calc lost job" stopDeciders.Token (fun () -> ignore <| this.EvaluteState())

        member private this.generateNewJobs() = 
            let removeJob jobid =
                let metaAction = RemoveJob jobid
                let removejobMsg = buildIilSendMessage (this.simulationID,metaAction)
                SendAgentServerEvent.Trigger (this, new UnaryValueEvent<IilAction>(removejobMsg))
        
            let createJob job =
                let metaAction = 
                    match job with
                    | ((Some jobid,_,_,_),_) -> UpdateJob job
                    | ((None,_,_,_),_) -> CreateJob job
                let jobMsg = buildIilSendMessage (this.simulationID,metaAction)
                SendAgentServerEvent.Trigger (this, new UnaryValueEvent<IilAction>(jobMsg))

            let jobTypes = Enum.GetValues(typeof<JobType>)
            let rec jobGenFunc jobtype state knownJobs =
                let (created,removed) = generateJob jobtype state knownJobs

                
                List.iter removeJob removed
                List.iter createJob created
                ()
//                if not (List.isEmpty created && List.isEmpty removed) then
//                    let removedKnown = List.filter (fun ((id,_,_,_),_) -> not (List.exists (fun jid -> if (id:Option<JobID>).IsSome then id.Value = jid else false) removed) ) knownJobs
//                    let createdKnown = List.filter (fun ((id,_,_,_),_) -> not (List.exists (fun cjob ->
//                                                (  match cjob with
//                                                     | (Some cjid,_,_,_),_ -> if (id:Option<JobID>).IsSome then id.Value = cjid else false
//                                                     | (None,_,_,_),_ -> false
//                                                )
//                                                ) created) ) removedKnown
//
//                    logImportant (sprintf "createdKnown: %A" createdKnown)
//                    logImportant (sprintf "removedKnown: %A" removedKnown)
//                
//                    jobGenFunc jobtype state (created@createdKnown)
//                else
//                    //logInfo ("Generating job completed: "+jobtype.ToString())
//                    ()
            let jobTypeList = (List.ofSeq (jobTypes.Cast<JobType>())).Tail
            let joblist = lock knownJobsLock (fun () -> this.KnownJobs)
            let knownjobs jobtype =List.filter (fun ((_, _, jt,_), _) -> jt = jobtype) joblist
            let stateData = lock stateLock (fun () -> this.BeliefData)

            this.asyncCalculationMany runningCalcID "calc generate job" 
                    (fun jobType -> ignore <| jobGenFunc jobType stateData (knownjobs jobType)) 
                    jobTypeList stopDeciders.Token
        
        member private this.EvaluateJob  job =
            let jobs = lock knownJobsLock (fun () -> this.KnownJobs)
            let eval id job () = 
                let changeCals value = 
                    lock runningCalcLock (fun () -> 
                        if id = runningCalcID then
                            jobDecideCalcs <- jobDecideCalcs + value
                            //logInfo ("Job Calcs: "+jobDecideCalcs.ToString())
                            if jobDecideCalcs = 0 then
                                logInfo "Sending empty job"
                                let sendmsg = buildIilSendMessage (this.simulationID,ApplyJob((-1),0))
                                SendAgentServerEvent.Trigger (this, new UnaryValueEvent<IilAction>(sendmsg))
                             )
                let stateData = lock stateLock (fun () -> this.BeliefData)
                let (desire,wantJob) = decideJob stateData job
                if wantJob then
                    let ((jobid,_,_,_),_) = job
                    //logInfo ("Job wanted "+job.ToString())
                    let sendmsg = buildIilSendMessage (this.simulationID,ApplyJob((jobid.Value),desire))
                    SendAgentServerEvent.Trigger (this, new UnaryValueEvent<IilAction>(sendmsg))  
                changeCals (-1)

            let calcID = lock runningCalcLock ( fun () -> runningCalcID)
            this.asyncCalculation calcID "evaluate job" stopDeciders.Token (eval calcID job) 

        member private this.updateJob job =
            lock knownJobsLock (fun () -> 
                                    let filteredJobs = List.filter (fun ((Some jid,_,_,_),_) ->  
                                                                match job with
                                                                | ((Some id,_,_,_),_) -> not (id = jid)
                                                                | _ -> true) this.KnownJobs 
                                     
                                    this.KnownJobs <- job::filteredJobs)
            
            this.EvaluateJob job


        interface IAgentLogic with
            member this.SetGoal goal =
                lock stateLock (fun () -> this.BeliefData <- { this.BeliefData with Goals =  [goal] })

            member this.Close() = 
                stopLogic()
                ()

            member this.HandlePercepts(iilpercepts) = 
                if simEnded then
                    ()
                let ServerMessage = this.protectedExecute ("Parsing percepts",(fun () -> Some (parseIilPercepts iilpercepts)),(fun () -> None))
                match ServerMessage with
                | Some (AgentServerMessage msg) ->
                    match msg with
                    | AddedOrChangedJob job ->
                        this.updateJob job
                    | AcceptedJob (id,vn) ->
                        logImportant ("Recieved Job: "+id.ToString())
                        
                        this.CalculateAcceptedJob (Some id) vn
                        recievedJobFromServer <- true
                    | SharedPercepts percepts ->
                        ignore <| lock awaitingPerceptsLock (fun () -> this.awaitingPercepts <- percepts@this.awaitingPercepts)
                    | RoundChanged id ->
                        ()
                    | RemovedJob ((Some jobid,_,_,_),_) -> 
                        lock knownJobsLock (fun () -> 
                            this.KnownJobs <- List.filter (fun ((kid,_,_,_),_) -> 
                                                                    match kid with
                                                                    | Some id -> id <> jobid
                                                                    | _ -> true
                                                                ) this.KnownJobs
                            ())
                    | unknown -> logError ("Master server message unintelligible: "+unknown.ToString())
                | Some (MarsServerMessage msg) ->
                    match msg with
                    | SimulationEnd _ -> 
                        SimulationEndedEvent.Trigger(this, new EventArgs())
                        stopLogic()
                        ()                        
                    | SimulationStart sData ->
                        this.KnownJobs <- []
                        this.awaitingPercepts <- []
                        this.simulationID <- sData.SimId
                        let subscribeAction = buildIilSendMessage (this.simulationID, SimulationSubscribe)
                        SendAgentServerEvent.Trigger(this, new UnaryValueEvent<IilAction>(subscribeAction))
                        this.BeliefData <- buildInitState (agentname, sData)
                    | ActionRequest ((deadline, actionTime, id), percepts) ->
                        
                        this.ReEvaluate percepts

                        let knownJobs = lock knownJobsLock (fun () -> this.KnownJobs)
                        if List.isEmpty knownJobs then
                            let sendmsg = buildIilSendMessage (this.simulationID,ApplyJob((-1),(-1)))
                            SendAgentServerEvent.Trigger (this, new UnaryValueEvent<IilAction>(sendmsg))
                        else
                            lock runningCalcLock (fun () -> jobDecideCalcs <- List.length knownJobs)
                            List.iter this.EvaluateJob knownJobs

                        this.generateNewJobs()
                        let totalTime = deadline - actionTime
                        let forceDecision start totaltime =
                            async
                                {
                                    
                                    let! token = Async.CancellationToken
                                    
                                    let awaitingDecision = ref true
                                    while awaitingDecision.Value do 
                                        if token.IsCancellationRequested then
                                            awaitingDecision:=false
                                        else                                          
                                            do! Async.Sleep(100)
                                            let expired = (System.DateTime.Now.Ticks - start)/(int64(10000))
                                        
                                            let runningCalcs = lock runningCalcLock (fun () -> runningCalc)
                                            logImportant ("Current running calculations: "+runningCalcs.ToString())
                                            logImportant ("Running: "+runningCalcNames.ToString())
                                            if (runningCalcs = 0) || (expired+int64(800)) > int64(totaltime) then
                                                let (_,decided) = lock decisionLock (fun () -> this.DecidedAction)
                                                if (decided <> Skip) then
                                                    SendMarsServerEvent.Trigger(this,new UnaryValueEvent<IilAction>(buildIilAction (float id) (lock decisionLock (fun () -> snd decidedAction))))
                                                    awaitingDecision:=false
                                                
                                            
                                }
                        Async.Start ((forceDecision System.DateTime.Now.Ticks totalTime),stopDeciders.Token)
                        
                        ()
                    | ServerClosed -> ()
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