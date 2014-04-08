using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using XmasEngineModel.Management;

namespace NabfProject.Events
{
    public class RoundChangedEvent : XmasEvent
    {
        public int Round { get; set; }

        public RoundChangedEvent(int Round)
        {
            this.Round = Round;
        }

    }
}
