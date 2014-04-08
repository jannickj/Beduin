using NabfProject.ServerMessages;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;
using XmasEngineController;

namespace NabfProject.AI
{
	public class ServerApplication : XmasController
	{
		private ServerCommunication communication;
        private TcpClient tcpClient;
        private int nrOfAgents;
        private string agentName;
        private string[] passwords;

		public ServerApplication(ServerCommunication communication, int nrOfAgents, string agentName, params string[] passwords)
		{
			this.communication = communication;
            //this.tcpClient = tcpClient;
            this.nrOfAgents = nrOfAgents;
            this.agentName = agentName;
            this.passwords = passwords;
		}

		public override void Initialize()
		{
			base.Initialize();
		}

		public override void Start()
		{
			base.Start();

            Authenticate();
            
            
		}

        private void Authenticate()
        {
            for (int i = 1; i <= nrOfAgents; i++)
            {
                var message = new AuthenticationRequestMessage(agentName + i, passwords[i - 1]);

                communication.SeralizePacket(message);

                var receiveMessage = (AuthenticationResponseMessage)communication.DeserializeMessage();

                if (receiveMessage.Response == ServerResponseTypes.Success)
                    Console.WriteLine(agentName + i + " connected to server.");
                else
                    Console.WriteLine(agentName + i + " failed to connect to server.");
            }
        }
	}
}
