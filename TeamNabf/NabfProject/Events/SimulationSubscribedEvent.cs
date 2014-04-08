using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using XmasEngineModel.Management;

namespace NabfProject.Events
{
    public class SimulationSubscribedEvent : XmasEvent
    {
        public int SimId { get; private set; }

        public SimulationSubscribedEvent(int simID)
        {
            SimId = simID;
        }
    }
}
