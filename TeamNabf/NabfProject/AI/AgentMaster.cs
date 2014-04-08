using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using JSLibrary.IiLang;
using JSLibrary.IiLang.DataContainers;
using NabfProject.Actions;
using XmasEngineController.AI;
using XmasEngineModel;
using XmasEngineModel.EntityLib;
using System.IO;
using JSLibrary.IiLang.Parameters;
using NabfProject.Parsers;
using System.Threading;
using XmasEngineModel.Management;
using XmasEngineModel.Management.Events;
using XmasEngineModel.Management.Actions;

namespace NabfProject.AI
{
	public class AgentMaster : AgentManager
	{
		private TcpListener listener;
        private Dictionary<string, Agent> agents = new Dictionary<string, Agent>();

		public AgentMaster(TcpListener listener)
		{
			this.listener = listener;
		}

		protected override Func<KeyValuePair<string, AgentController>> AquireAgentControllerContructor()
		{
			TcpClient client = listener.AcceptTcpClient();
			Func<KeyValuePair<string, AgentController>> func = () => 
			{ 
                
				
                //XmlReader reader = XmlReader.Create(client.GetStream(), new XmlReaderSettings() { ConformanceLevel = ConformanceLevel.Fragment });
				//XmlWriter writer = XmlWriter.Create(client.GetStream(),new XmlWriterSettings(){ConformanceLevel = System.Xml.ConformanceLevel.Fragment});
                StreamReader sreader = new StreamReader(client.GetStream(), Encoding.UTF8);
                StreamWriter swriter = new StreamWriter(client.GetStream(), Encoding.UTF8);
				XmlPacketTransmitter<IilAction, IilPerceptCollection> transmitter = new XmlPacketTransmitter<IilAction, IilPerceptCollection>(sreader, swriter);
                
                IilAction action = transmitter.DeserializeMessage();

                string agentName = ((IilIdentifier)action.Parameters[0]).Value;

                Agent agent;
                lock (agents)
                {
                    if (!agents.TryGetValue(agentName, out agent))
                    {
                        agent = new NabfAgent(agentName);
                        agents.Add(agentName, agent);
                        var agentAdded = new AutoResetEvent(false);
                        var trigger = new Trigger<ActionCompletedEvent<AddXmasObjectAction>>(evt => { if (evt.Action.Object == agent) agentAdded.Set(); });
                        this.EventManager.Register(trigger);
                        this.ActionManager.Queue(new XmasEngineModel.Management.Actions.AddXmasObjectAction(agent));
                        agentAdded.WaitOne();
                        this.EventManager.Deregister(trigger);


                    }

                }


				AgentConnection connection = new AgentConnection(agent,transmitter,new AgentMasterToAgentParser(), new AgentToAgentMasterParser());               

				return new KeyValuePair<string,AgentController>(agentName, connection);
			};
			return func;
		}
	}
}
