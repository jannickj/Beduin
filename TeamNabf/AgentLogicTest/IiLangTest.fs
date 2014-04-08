namespace AgentLogicTest
open System
open JSLibrary.IiLang
open JSLibrary.IiLang.Parameters
open JSLibrary.IiLang.DataContainers
open NUnit.Framework
open NabfAgentLogic.IiLang.IiLangDefinitions

[<TestFixture>]
type IiLangTest() = 

        [<Test>]
        member this.ReadSimpleIil_EmptyPercept_EmptyPercept () =
            let testName = "testPercept"
            let emptyPercept = new IilPercept (testName)
            let expected = Percept (testName, List.empty<Element>)
            let actual = parsePercept emptyPercept
            Assert.AreEqual (expected, actual)

        [<Test>]
        member this.ReadPerceptWithIdentifier () =
            let testName = "testPercept"
            let testId = "testId"
            let identifierPercept = new IilPercept (testName, new IilIdentifier (testId))

            let expected = Percept (testName, [Identifier testId])
            let actual = parsePercept identifierPercept

            Assert.AreEqual (expected, actual)

        [<Test>]
        member this.ReadPerceptWithNumeral () =
            let testName = "testPercept"
            let testNum = 1.
            let numeralPercept = new IilPercept (testName, new IilNumeral (testNum))

            let expected = Percept (testName, [Numeral testNum])
            let actual = parsePercept numeralPercept

            Assert.AreEqual (expected, actual)

        [<Test>]
        member this.ReadPerceptWithEmptyParameterList () =
            let testName = "testPercept"
            let emptyParamListPercept = new IilPercept (testName, new IilParameterList ())

            let expected = Percept (testName, [ParameterList []])
            let actual = parsePercept emptyParamListPercept

            Assert.AreEqual (expected, actual)

        [<Test>]
        member this.ReadPerceptWithDeepParameterList () =
            let testName = "testPercept"
            let testId = "testId"
            let testNum = 1.

            let iilId = new IilIdentifier (testId)
            let iilNum = new IilNumeral (testNum)
            let iilInnerParamList = new IilParameterList (iilId)
            let iilOuterParamList = new IilParameterList (iilInnerParamList, iilNum)
            let deepParamListPercept = new IilPercept (testName, iilOuterParamList)

            let innerParamList = ParameterList [Identifier testId]
            let outerParamList = ParameterList [innerParamList; Numeral testNum]

            let expected = Percept (testName, [outerParamList])
            let actual = parsePercept deepParamListPercept

            Assert.AreEqual (expected, actual)

        [<Test>]
        member this.ReadPerceptWithNamedEmptyFunction () =
            let testName = "testPercept"
            let funName = "testFun"

            let iilTestFun = new IilFunction (funName)
            let iilEmptyFunPercept = new IilPercept (testName, iilTestFun)

            let expected = Percept (testName, [Function (funName, [])])
            let actual = parsePercept iilEmptyFunPercept

            Assert.AreEqual (expected, actual)

        [<Test>]
        member this.ReadPerceptWithMultipleParams () =
            let testName = "testPercept"
            let testId = "testId"
            let testNum = 1.

            let iilTestId = new IilIdentifier (testId)
            let iilTestNum = new IilNumeral (testNum)
            let multiParamPercept = new IilPercept (testName, iilTestNum, iilTestId)

            let expected = Percept (testName, [Numeral testNum; Identifier testId])
            let actual = parsePercept multiParamPercept

            Assert.AreEqual (expected, actual)


