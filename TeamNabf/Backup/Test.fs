
namespace AgentLogicTest
open System
open NUnit.Framework
open AgentLogic

[<TestFixture>]
type Test() = 
//        [<Test>]
//        abstract TestCase : unit -> unit

        [<Test>]
        member this.TestSomething() =
            Assert.AreEqual (10, multify 5 2)
        
