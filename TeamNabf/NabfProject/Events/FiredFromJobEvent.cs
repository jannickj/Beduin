using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NabfProject.NoticeBoardModel;
using XmasEngineModel.Management;
using NabfProject.AI;

namespace NabfProject.Events
{
    public class FiredFromJobEvent : XmasEvent
    {
        public Notice Notice { get; private set; }
        public NabfAgent Receiver { get; private set; }

        public FiredFromJobEvent(Notice n, NabfAgent agent)
        {
            Notice = n;
            Receiver = agent;
        }
    }
}
