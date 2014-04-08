using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using NabfProject.AI;
using XmasEngineModel.EntityLib;
using XmasEngineModel.Management;

namespace NabfProject.Actions
{
	public class AddStandbyAgentAction : EnvironmentAction
	{
		private Agent agent;

		public AddStandbyAgentAction(Agent agent)
		{
			this.agent = agent;
		}

		protected override void Execute()
		{
			throw new NotImplementedException();
		}
	}
}
