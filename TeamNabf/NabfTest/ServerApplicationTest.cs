using NabfProject.AI;
using NabfProject.ServerMessages;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Xml;
using System.Xml.Serialization;

namespace NabfTest
{
    [TestFixture]
    public class ServerApplicationTest
    {
        


        [Test]
        public void ConnectToServerAndAuthenticate_Connected_AllAgentsAuthenticated()
        {
            Assert.Pass("Test disabled");
            StreamReader reader;
            StreamWriter writer;
            ServerCommunication servCom;
            TcpClient tcpClient;
            int nrOfAgents = 28;

            //tcpClient = new TcpClient();
            //tcpClient.Connect("localhost", 12300);

            //Stream stream = tcpClient.GetStream();

            //StreamWriter streamWriter = new StreamWriter(stream);
            //StreamReader streamReader = new StreamReader(stream);

            //servCom = new ServerCommunication(streamReader, streamWriter);            

            for (int i = 1; i <= nrOfAgents; i++)
            {
                tcpClient = new TcpClient();
                tcpClient.Connect("localhost", 12300);

                Stream stream = tcpClient.GetStream();

                StreamWriter streamWriter = new StreamWriter(stream);
                StreamReader streamReader = new StreamReader(stream);

                servCom = new ServerCommunication(streamReader, streamWriter);
                servCom.SeralizePacket(new AuthenticationRequestMessage("Nabf" + i, "pass"));

                var authMessage = servCom.DeserializeMessage();

                var simStartMessage = servCom.DeserializeMessage();
            }

            
            

            //int id = Convert.ToInt32(((SimStartMessage)simStartMessage).Response["id"]);
            //int simstartid = id;

            //var actionReqMessage = (RequestActionMessage)servCom.DeserializeMessage();

            //id = ((PerceptionMessage)actionReqMessage.Response).Id;
            //int firstactionid = id;

            //servCom.SeralizePacket(new ActionMessage(id, "recharge"));

            //actionReqMessage = (RequestActionMessage)servCom.DeserializeMessage();

            //id = ((PerceptionMessage)actionReqMessage.Response).Id;
            //int secondactionid = id;
            



            //XmlWriter xmlWriter = XmlWriter.Create(streamWrite);

            //servCom = new ServerCommunication(xmlWriter, null, xmlSerSend, xmlSerReceive);
            //servCom.TestStream = tcpClient.GetStream();

            //ServerApplication servApp = new ServerApplication(servCom, nrOfAgents, agentName, passwords);

            //servApp.Start();
            
        }

        [Test]
        public void SendActionToServer_Connected_ReceiveLastActionResultSuccessful()
        {
            Assert.Pass("Test disabled");
            StreamReader reader;
            StreamWriter writer;
            ServerCommunication servCom;
            TcpClient tcpClient;
            int nrOfAgents = 1;        

            tcpClient = new TcpClient();
            tcpClient.Connect("localhost", 12300);

            Stream stream = tcpClient.GetStream();

            StreamWriter streamWriter = new StreamWriter(stream);
            StreamReader streamReader = new StreamReader(stream);

            servCom = new ServerCommunication(streamReader, streamWriter);
            servCom.SeralizePacket(new AuthenticationRequestMessage("Nabf" + 1, "pass"));

            var authMessage = servCom.DeserializeMessage();


            var simStartMessage = servCom.DeserializeMessage();

			var simMsg = (SimStartMessage)simStartMessage;

            int id = simMsg.Id;
            int simstartid = id;

            var actionReqMessage = (RequestActionMessage)servCom.DeserializeMessage();

            id = ((PerceptionMessage)actionReqMessage.Response).Id;
            int firstactionid = id;

            servCom.SeralizePacket(new ActionMessage(id, "recharge"));

            actionReqMessage = (RequestActionMessage)servCom.DeserializeMessage();

            string lastAction = ((SelfMessage)((PerceptionMessage)actionReqMessage.Response).Elements[1]).LastAction;
            string lastActionResult = ((SelfMessage)((PerceptionMessage)actionReqMessage.Response).Elements[1]).LastActionResult;
            Assert.AreEqual("successful", lastActionResult);
            Assert.AreEqual("recharge", lastAction);


        }

        [Test]
        public void AuthenticateAllAgentsSeperateThreads_Connected_AllAgentsAuthenticated()
        {
            int nrOfAgents = 28;
            Thread[] threads = new Thread[nrOfAgents];
            ParameterizedThreadStart[] starters = new ParameterizedThreadStart[nrOfAgents];

            
            int i = 1;

            for (i = 1; i <= nrOfAgents; i++)
            {
                var nr = i;

                starters[i - 1] = delegate { RunSimulationAllSameAction(nr); };

                threads[i - 1] = new Thread(starters[i - 1]);
                threads[i - 1].Name = "TESTER" + i;
                threads[i - 1].Start();

                while (!threads[i - 1].IsAlive) { }


            }

            Assert.AreEqual(nrOfAgents, i-1);

            for (i = 0; i < nrOfAgents; i++)
            {
                threads[i].Join();
            }
            
            Assert.Pass();


        }
        
        private void RunSimulationSingleAction(int i)
        {
            TcpClient tcpClient2 = new TcpClient();
            tcpClient2.Connect("localhost", 12300);

            Stream stream = tcpClient2.GetStream();

            StreamWriter streamWriter = new StreamWriter(stream);
            StreamReader streamReader = new StreamReader(stream);
            ServerCommunication servCom = new ServerCommunication(streamReader, streamWriter);

            AuthenticateToServer(i, servCom);

            int id = ReceiveSimStart(servCom);

            var actionReqMessage = (RequestActionMessage)servCom.DeserializeMessage();

            id = ((PerceptionMessage)actionReqMessage.Response).Id;
            int firstactionid = id;

            servCom.SeralizePacket(new ActionMessage(id, "recharge"));

            actionReqMessage = (RequestActionMessage)servCom.DeserializeMessage();

            id = ((PerceptionMessage)actionReqMessage.Response).Id;
            int secondactionid = id;
        }

        private void RunSimulationAllSameAction(int i)
        {
            TcpClient tcpClient2 = new TcpClient();
            tcpClient2.Connect("localhost", 12300);

            Stream stream = tcpClient2.GetStream();

            StreamWriter streamWriter = new StreamWriter(stream);
            StreamReader streamReader = new StreamReader(stream);
            ServerCommunication servCom = new ServerCommunication(streamReader, streamWriter);

            AuthenticateToServer(i, servCom);

            int id = ReceiveSimStart(servCom);

            RequestActionMessage actionReqMessage;

            while (true)
            {
                actionReqMessage = (RequestActionMessage)servCom.DeserializeMessage();

                id = ((PerceptionMessage)actionReqMessage.Response).Id;
                int firstactionid = id;

                servCom.SeralizePacket(new ActionMessage(id, "recharge"));
            }
            

        }

        private static int ReceiveSimStart(ServerCommunication servCom2)
        {
            var simStartMessage = ((SimStartMessage)servCom2.DeserializeMessage());

            int id = ((SimStartMessage)simStartMessage).Id;
            int simstartid = id;
            return id;
        }

        private static void AuthenticateToServer(int i, ServerCommunication servCom2)
        {
            servCom2.SeralizePacket(new AuthenticationRequestMessage("Nabf" + i, "pass"));

            var authMessage = ((AuthenticationResponseMessage)servCom2.DeserializeMessage()).Response;
        }
    }

    public class TestThreads
    {
        private volatile bool shouldStop = false;
        public void TestStartThread()
        {
            Assert.True(false);
        }

        public void RequestStop()
        {
            shouldStop = true;
        }
    }
}
