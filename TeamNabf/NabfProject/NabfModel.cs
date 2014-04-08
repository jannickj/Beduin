using NabfProject.SimManager;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using XmasEngineModel;
using XmasEngineModel.Management;

namespace NabfProject
{
	public class NabfModel : XmasModel
	{
        public SimulationManager SimulationManager { get; private set; }

        public NabfModel(SimulationManager simMan, XmasWorldBuilder builder, ActionManager actman, EventManager evtman, XmasFactory factory) : base(builder,actman,evtman,factory)
		{
            this.SimulationManager = simMan;
            this.AddActor(simMan);
        }

	}
}
