using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JSLibrary.IiLang;
using JSLibrary.IiLang.DataContainers;
using XmasEngineController.AI;
using XmasEngineModel.EntityLib;
using XmasEngineModel.Management;
using JSLibrary.ParallelLib;
using System.Threading;
using NabfProject.Events;
using System.Xml.Linq;
using NabfProject.Parsers;
using NabfProject.Actions;

namespace NabfProject.AI
{
	public class AgentConnection : AgentController
	{
        private bool disconnected = false;
        private object simLock = new object();
        private bool simulationSubscribed = false;
        private ConcurrentQueue<IilPerceptCollection> packets = new ConcurrentQueue<IilPerceptCollection>();
        private AutoResetEvent packetadded = new AutoResetEvent(false);
		private XmlPacketTransmitter<IilAction, IilPerceptCollection> transmitter;
        private AgentMasterToAgentParser masterToAgentParser;
        private AgentToAgentMasterParser agentToMasterParser;

		public AgentConnection  (   Agent agent
                                ,   XmlPacketTransmitter<IilAction, IilPerceptCollection> transmitter
                                ,   AgentMasterToAgentParser masterToAgentParser
                                ,   AgentToAgentMasterParser agentToMasterParser
                                )
			: base(agent)
		{
			this.transmitter = transmitter;
            this.Agent.Register(new Trigger<NewKnowledgeEvent>(receivedEvent));
            this.Agent.Register(new Trigger<NewNoticeEvent>(receivedEvent));
            this.Agent.Register(new Trigger<NoticeRemovedEvent>(receivedEvent));
            this.Agent.Register(new Trigger<NoticeUpdatedEvent>(receivedEvent));
            this.Agent.Register(new Trigger<ReceivedJobEvent>(receivedEvent));
			this.Agent.Register(new Trigger<RoundChangedEvent> (receivedEvent));
			this.Agent.Register(new Trigger<FiredFromJobEvent> (receivedEvent));
            this.Agent.Register(new Trigger<SimulationSubscribedEvent>(simSubscribedEvent));

			this.masterToAgentParser = masterToAgentParser;
            this.agentToMasterParser = agentToMasterParser;
		}


		public override void Start()
		{
            var senderThread  = new Thread(new ThreadStart(() => this.Updater(updateSend)));
            var updaterThread = new Thread(new ThreadStart(() =>  this.Updater(updateReceive)));

            senderThread.Name = "Agent " + this.Agent.Name + ": Sender Thread";
            updaterThread.Name = "Agent " + this.Agent.Name + ": Updater Thread";

            senderThread.Start();
            updaterThread.Start();
		}

        private void Updater(Action action)
        {
            try
            {
                while (true)
                {
                    if (disconnected)
                        throw new Exception("Connection closed");
                    action();
                }
            }
            catch (Exception e)
            {
                lock (simLock)
                    this.disconnected = true;

                this.Agent.QueueAction(new AgentCrashed(e));
            }
        }

        private void updateReceive()
        {

            var action = transmitter.DeserializeMessage();
            var agent = (NabfAgent)this.Agent;
            var xaction = agentToMasterParser.ConvertToForeign(action);
            //NabfAgent agentToReceive;
            //if (xaction is AddKnowledgeAction)
            //{
            //    foreach (KnowledgeManagerModel.Knowledge k in ((AddKnowledgeAction)xaction).SentKnowledge)
            //    {
            //        if (k is KnowledgeManagerModel.MessageKnowledge)
            //        {
            //            //agentToReceive = ((KnowledgeManagerModel.MessageKnowledge)k).TargetedAgent
            //            //agentToReceive.Raise(new NewKnowledgeEvent(k));
            //        }
            //    }
            //}
            if (xaction is SubscribeSimulationAction)
                lock (simLock)
                    this.simulationSubscribed = false;
            bool noaction = false;
            lock (simLock)
                noaction = disconnected;
            if(!noaction)
                agent.QueueAction(xaction);
        }

        private void updateSend()
        {
            this.packetadded.WaitOne();
            bool hasPacket = false;
            do
            {
                IilPerceptCollection packet;
                hasPacket = this.packets.TryDequeue(out packet);
                if (hasPacket)
                {
                    this.transmitter.SeralizePacket(packet);
                }
            } while (hasPacket);
        }

        private void simSubscribedEvent(SimulationSubscribedEvent evt)
        {
            lock (simLock)
                this.simulationSubscribed = true;
        }

        private void receivedEvent(XmasEvent evt)
        {
            var packet = this.masterToAgentParser.ConvertToForeign(evt);

            lock (simLock)
                if (simulationSubscribed && !disconnected)
                    addPacket(packet);
        }

        private void addPacket(IilPerceptCollection packet)
        {
            this.packets.Enqueue(packet);
            this.packetadded.Set();
        }

	}
}
