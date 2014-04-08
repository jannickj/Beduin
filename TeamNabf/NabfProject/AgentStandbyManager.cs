using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using NabfProject.AI;
using XmasEngineModel.EntityLib;

namespace NabfProject
{
	public class AgentStandbyManager
	{
		private Queue<Agent> agents = new Queue<Agent>();


		public void AddAgent(Agent agent)
		{
			this.agents.Enqueue(agent);
		}

	}
}
