using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using XmasEngineModel.EntityLib;
using XmasEngineModel.Management;

namespace NabfProject.Actions
{
    public class AgentCrashed : EntityXmasAction<Agent>
    {
        public Exception Exception { get; private set; }

        public AgentCrashed(Exception e)
        {
            this.Exception = e;
        }

        protected override void Execute()
        {
        }
    }
}
