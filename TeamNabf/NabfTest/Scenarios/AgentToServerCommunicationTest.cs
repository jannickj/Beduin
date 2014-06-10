using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using JSLibrary.IiLang;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;
using NabfProject.Actions;
using NabfProject.AI;
using NabfProject.KnowledgeManagerModel;
using NabfProject.Parsers;
using NabfProject.Parsers.AgentToAgentMasterConverters;
using NUnit.Framework;

namespace NabfTest.Scenarios
{
    [TestFixture]
    public class AgentToServerCommunicationTest
    {

        [Test]
        public void WhatIsBeingTested_CurrentState_ExpectedResult()
        {
            Assert.Pass("Test disabled");
            TcpClient masterClient = new TcpClient();
            IilPerceptCollection ipc, ipc2;
            AgentMasterDataParsers amdp = new AgentMasterDataParsers();
            masterClient.Connect("localhost", 1337);

            XmlPacketTransmitter<IilPerceptCollection, IilAction> masterSerCom = new XmlPacketTransmitter<IilPerceptCollection, IilAction>(
                new StreamReader(masterClient.GetStream()),
                new StreamWriter(masterClient.GetStream()));
            int simID = 2;

            masterSerCom.SeralizePacket(new IilAction("hateand", new IilIdentifier("dumme and")));
            masterSerCom.SeralizePacket(new IilAction("subscribeSimulationAction", new IilNumeral(simID)));

            Assert.IsTrue(masterClient.Connected);

            int notice_id;
            int round_id;
            int jobtype = 1;
            int agents_needed = 1;
            int value = 9;
            int desire = 999;

            //var ie = (IilPerceptCollection)amdp.ConvertToForeign(new NodeKnowledge("node1"));
            var nodeKnowledge = new IilFunction("nodeKnowledge",new IilIdentifier("mahnodename"),new IilNumeral(23));
            var notepacket = new IilFunction(    "createNoticeAction"
                                            ,   new IilNumeral(simID)
                                            ,   new IilNumeral(jobtype)
                                            ,   new IilNumeral(agents_needed)
                                            ,   new IilFunction("nodes",nodeKnowledge)
                                            ,   new IilNumeral(value)
                                            ,   new IilFunction("zone",nodeKnowledge));


            
            //masterSerCom.SeralizePacket(new IilAction("addKnowledgeAction", new IilFunction("fuu", new IilNumeral(2), new IilFunction("nod", new IilIdentifier( ) ));
            masterSerCom.SeralizePacket(new IilAction("createNoticeAction", notepacket));

            ipc = masterSerCom.DeserializeMessage();
            //System.Diagnostics.Debugger.Break();

            notice_id = 0;
            round_id = 10;

            var newroundpacket = new IilFunction("newRoundAction"
                                            , new IilNumeral(simID)
                                            , new IilNumeral(round_id)
                                            );

            var applypacket = new IilFunction("applyNoticeAction"
                                            , new IilNumeral(simID)
                                            , new IilNumeral(notice_id)
                                            , new IilNumeral(desire)
                                            );
            var finishedapplypacket = new IilFunction("applyNoticeAction"
                                            , new IilNumeral(simID)
                                            , new IilNumeral(-1)
                                            , new IilNumeral(desire)
                                            );

            masterSerCom.SeralizePacket(new IilAction("newRoundAction", newroundpacket));
            masterSerCom.SeralizePacket(new IilAction("applyNoticeAction", applypacket));
            //masterSerCom.SeralizePacket(new IilAction("applyNoticeAction", finishedapplypacket));

            ipc2 = masterSerCom.DeserializeMessage();
            System.Diagnostics.Debugger.Break();

            //while (true) { Thread.Sleep(100); }
        }
    }
}
