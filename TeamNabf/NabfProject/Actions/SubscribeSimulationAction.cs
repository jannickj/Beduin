using NabfProject.AI;
using NabfProject.SimManager;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using XmasEngineModel.EntityLib;
using XmasEngineModel.Management;

namespace NabfProject.Actions
{
    public class SubscribeSimulationAction : EntityXmasAction<NabfAgent>
    {
        private int SimId;

        public SubscribeSimulationAction(int simID)
        {
            SimId = simID;
        }

        protected override void Execute()
        {
            SimulationManager simMan = ((NabfModel)this.Engine).SimulationManager;

            simMan.SubscribeToSimulation(SimId, this.Source);
        }
    }
}
