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
	public class ClientApplication : IStartable
	{
        protected readonly object simLock = new object();
		private int currentSimId = -1;

		
		private IAgentLogic currentLogic;


        private AgentLogicFactory logicFactory;
		//private XmlPacketTransmitter<IilPerceptCollection,IilAction> agentServCom;
		private HashSet<Thread> activeThreads = new HashSet<Thread>();
        private List<Tuple<int, InternalSendMessage>> marsPackets = new List<Tuple<int, InternalSendMessage>>();
		private AutoResetEvent marsPacketAdded = new AutoResetEvent(false);
        private MarsToAgentParser marsToAgentParser;
        private AgentToMarsParser agentToMarsParser;
        private DateTime actionTimeStart;

		private ServerCommunication marsServCom;
        public event UnaryValueHandler<Tuple<ActionMessage,TimeSpan>> ActionSent;

		public IAgentLogic CurrentLogic
		{
		  get { return currentLogic; }
		}
		public int CurrentSimId
		{
			get { return currentSimId; }
		}

		public ClientApplication(ServerCommunication marsServCom, MarsToAgentParser marsParser, AgentToMarsParser agentToMarsParser, AgentLogicFactory factory)
		{
			
			this.marsServCom = marsServCom;
            this.marsToAgentParser = marsParser;
            this.agentToMarsParser = agentToMarsParser;
            //this.agentServCom = agentServCom;
            this.logicFactory = factory;
            //logic.EvaluationStarted += logic_needMessageSent;
            actionTimeStart = DateTime.Now;
		}

		public void SetGoal(NabfAgentLogic.AgentTypes.Command goal)
		{
			this.currentLogic.SetGoal(goal);
		}

		public void UpdateMarsSender()
		{
			this.marsPacketAdded.WaitOne();

            Tuple<int, InternalSendMessage>[] packets;
            lock (marsPackets)
            {
                packets = this.marsPackets.ToArray();
                this.marsPackets.Clear();
            }

            foreach(var packet in packets)
            {
			    bool packetAccepted = false;
                lock (simLock)
                {
                    if (packet.Item1 == this.currentSimId)
                        packetAccepted = true;
                }
                if (ActionSent != null)
                {
                    var evtArgs = Tuple.Create((ActionMessage)packet.Item2, DateTime.Now - actionTimeStart);
                            
                    ActionSent(this, new UnaryValueEvent<Tuple<ActionMessage, TimeSpan>>(evtArgs));
                }

                if (packetAccepted)
                    this.marsServCom.SeralizePacket(packet.Item2);
                
			}

		}


		public void UpdateMarsReceiver()
		{
            var data = (ReceiveMessage) marsServCom.DeserializePacket();
            IAgentLogic logic;
            lock (simLock)
            {
                logic = this.currentLogic;
            

                if (data.Message is SimStartMessage)
                {
                    StartSim((SimStartMessage)data.Message);
                    logic = this.currentLogic;
                }
                else if (data.Message is RequestActionMessage)
                {
                    actionTimeStart = DateTime.Now;
                }
            }

            var percepts = (IilPerceptCollection) marsToAgentParser.ConvertToForeign(data);

            if (percepts.Percepts.Count != 0)
			{
                lock (logic)
                {
                    logic.HandlePercepts(percepts);
                }
			}
            
		}


		private void StartThread(Action action)
		{
			var thread = new Thread(new ThreadStart(action));
			lock (activeThreads)
			{
				this.activeThreads.Add(thread);
			}
			thread.Start();
		}


		private void AddMarsPacket(int id, InternalSendMessage packet)
		{
            lock(this.marsPackets)
			    this.marsPackets.Add(Tuple.Create(id,packet));
			this.marsPacketAdded.Set();
		}

		protected virtual void OnStartSim()
		{

		}

        private void StartSim(SimStartMessage msg)
        {
            lock (simLock)
            {
                
                currentSimId = msg.Id;
                currentLogic = logicFactory.ConstructAgentLogic();

                int simId = currentSimId;
                
                currentLogic.SendMarsServerAction += (sender,evt) => 
                    {
                        bool acceptedPacket = false;
                        lock(simLock)
                        {
                            if (currentSimId == simId)
                                acceptedPacket = true;
                        }
                        if (acceptedPacket)
                        {
                            var marsMsg = agentToMarsParser.ConvertToForeign(evt.Value);
                            this.AddMarsPacket(currentSimId, marsMsg);
                        }
                    };
				OnStartSim();

            }

            
        }

		protected virtual void OnStart()
		{

		}

        public void Start()
        {
			this.currentLogic = this.logicFactory.ConstructAgentLogic ();
            ReceiveMessage msg = (ReceiveMessage) marsServCom.DeserializePacket();
            if(msg.Message is SimEndMessage)
                msg = (ReceiveMessage)marsServCom.DeserializePacket();
            SimStartMessage sMsg = (SimStartMessage) msg.Message;
            var sMsgPercepts = (IilPerceptCollection) marsToAgentParser.ConvertToForeign(msg);
            StartSim(sMsg);
            currentLogic.HandlePercepts(sMsgPercepts);

			RunThread(this.UpdateMarsReceiver);
			RunThread(this.UpdateMarsSender);
			OnStart();

        }

		protected void RunThread(Action updater)
		{
			Thread thread = new Thread(new ThreadStart(() => { while (true) updater(); }));
			thread.Start();
		}

        private void ExecuteThread(Action action)
        {
            try
            {
                action();
            }
            catch (Exception e)
            {
                Logging.log (DebugLevel.Critical, e.Message + "\n" + e.StackTrace);
                Environment.Exit(1); 
            }
        }

        

        public void Initialize()
        {
            
        }
    }
}
