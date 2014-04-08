using NabfProject.KnowledgeManagerModel;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using XmasEngineModel.Management;

namespace NabfProject.Events
{
    public class NewKnowledgeEvent : XmasEvent
    {
        public Knowledge NewKnowledge;

        public NewKnowledgeEvent(Knowledge k)
        {
            NewKnowledge = k;
        }
    }
}
