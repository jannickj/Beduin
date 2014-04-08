module AgentLogicClientTest
    open System
    open NUnit.Framework
    open NabfAgentLogic.AgentTypes
    open NabfAgentLogic.AgentLogic
    open NabfAgentLogic.AgentInterfaces
    open JSLibrary.Data.GenericEvents
    open JSLibrary.IiLang
    open JSLibrary.IiLang.DataContainers
    open System.Threading
    open System.Linq
    open NabfAgentLogic

    [<TestFixture>]
    type AgentLogicClientTest() = 
        
        [<Test>]
        member this.EvaluateState_twoDecisions_ConcludesFirst() = 
            let waiter = new AutoResetEvent(false)
            let optionA state =
                Thread.Sleep 100
                ignore <| waiter.Set()
                (true,Some Action.Recharge)
            let optionB state =
                Thread.Sleep 10
                (true,Some Action.Parry)
            
            let tree= Options 
                        [
                            Choice(optionA)
                            Choice(optionB)
                        ]
            
            let client = new AgentLogicClient("",tree)
            let iclient = client :> IAgentLogic 
            client.EvaluteState()
            ignore <| waiter.WaitOne(1000)
            Thread.Sleep 10
            let (d,a) = client.DecidedAction
            Assert.AreEqual(Action.Recharge,a)
            Assert.AreEqual(0,d)

        [<Test>]
        member this.EvaluateState_twoDecisions_CancelsSecond() = 
            let bLocked = ref false
            let lockReleased = ref false
            let waiter = new AutoResetEvent(false)
            let locker = new Object()
            let optionA state =
                Thread.Sleep 100
                (true,Some Action.Recharge)
            let optionB state =
                lock locker (fun () ->  ignore <| waiter.Set()
                                        bLocked := true
                                        while true do () )
                (true,Some Action.Parry)
            
            let tree= Options 
                        [
                            Choice(optionA)
                            Choice(optionB)
                        ]
            let client = new AgentLogicClient("",tree)
            client.EvaluteState()
            
            
            ignore <| waiter.WaitOne()
            lock locker (fun () -> lockReleased := bLocked.Value)
            let (d,a) = client.DecidedAction
            Assert.IsTrue ((lockReleased.Value) && bLocked.Value)
