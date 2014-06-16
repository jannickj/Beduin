using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Xml;
using System.Xml.Serialization;
using JSLibrary.IiLang;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;
using NabfAgentLogic.AgentInterfaces;
using System.Collections.Concurrent;
using NabfAgentLogic;
using JSLibrary.Data.GenericEvents;
using XmasEngineModel.Interfaces;
using NabfProject.ServerMessages;
using NabfProject.Parsers;
using NabfProject.AI;
using NabfAgentLogic;
using JSLibrary.Logging;

namespace NabfClientApplication.Client
{
	public class ClientApplicationWithMaster : ClientApplication
	{
       
        private List<Tuple<int, IilAction>> masterPackets = new List<Tuple<int, IilAction>>();
        private AutoResetEvent masterPacketAdded = new AutoResetEvent(false);
        private XmlPacketTransmitter<IilPerceptCollection, IilAction> masterSerCom;


		public ClientApplicationWithMaster(XmlPacketTransmitter<IilPerceptCollection, IilAction> masterSerCom, ServerCommunication marsServCom, MarsToAgentParser marsParser, AgentToMarsParser agentToMarsParser, AgentLogicFactory factory)
			: base(marsServCom, marsParser, agentToMarsParser, factory)
		{
            this.masterSerCom = masterSerCom;
            
		}


        public void UpdateMasterSender()
        {
            this.masterPacketAdded.WaitOne();

            Tuple<int, IilAction>[] packets;
            lock (masterPackets)
            {
                packets = this.masterPackets.ToArray();
                this.masterPackets.Clear();
            }

            foreach (var packet in packets)
            {
                bool packetAccepted = false;
                lock (simLock)
                {
                    if (packet.Item1 == this.CurrentSimId)
                        packetAccepted = true;
                }
               
                if (packetAccepted)
                    this.masterSerCom.SeralizePacket(packet.Item2);

            }

        }

		

        public void UpdateMasterReceiver()
        {
            var percepts = masterSerCom.DeserializeMessage();
            

            if (percepts.Percepts.Count != 0)
            {
                lock (this.CurrentLogic)
                {
                    this.CurrentLogic.HandlePercepts(percepts);
                }
            }

        }



        private void AddMasterPacket(int id, IilAction packet)
        {
            lock(this.masterPackets)
                this.masterPackets.Add(Tuple.Create(id, packet));
            this.masterPacketAdded.Set();
        }
		protected override void OnStartSim()
		{
			base.OnStartSim();
			CurrentLogic.SendAgentServerAction += (sender, evt) =>
			{
				lock (simLock)
				{
					bool acceptedPacket = false;
					lock (simLock)
					{
						if (CurrentSimId == this.CurrentSimId)
							acceptedPacket = true;
					}
					if (acceptedPacket)
					{
						this.AddMasterPacket(CurrentSimId, evt.Value);
					}
				}
			};
		}



		protected override void OnStart()
		{
			this.RunThread(this.UpdateMasterReceiver,true);
			this.RunThread(this.UpdateMasterSender,true);
		}

       

    }
}
