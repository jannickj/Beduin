
namespace AgentLogicTest
    open System
    open NUnit.Framework
    open Graphing.Graph
    open NabfAgentLogic.AgentTypes
    open StructureBuilder
    open NabfAgentLogic.Explorer
    open NabfAgentLogic.Planning
    open FsPlanning.Search
    open NabfAgentLogic.LogicLib
    open NabfAgentLogic.Common
    open NabfAgentLogic.Search

    [<TestFixture>]
    type AnalyzePerceptsTest() = 
        
        let me = 1
