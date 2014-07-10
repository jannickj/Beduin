using JSLibrary.Conversion;
using JSLibrary.IiLang;
using NabfProject.Events;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;
using NabfProject.AI;

namespace NabfProject.Parsers.AgentMasterToAgentConverters
{
    public class ConverterReceivedJob : JSConverterToForeign<ReceivedJobEvent, IilPerceptCollection>
    {
        public AgentMasterDataParsers Parsers { get; set; }

        public override IilPerceptCollection BeginConversionToForeign(ReceivedJobEvent gobj)
        {
            IilPerceptCollection ipc;
            //IilPercept percept = ((IilPerceptCollection)Parsers.ConvertToForeign(gobj.Notice)).Percepts[0];

            SortedList<long, NabfAgent> sl = new SortedList<long, NabfAgent>();

            foreach (NabfAgent a in gobj.Notice.GetAgentsOnJob())
                sl.Add(a.Id, a);

            int receiverIndex = sl.IndexOfValue(gobj.Receiver); ; //gobj.Notice.GetTopDesireAgents().IndexOf(gobj.Receiver);
            string gotoNode = "emptyjob";
            if (gobj.Notice.WhichNodes.Count != 0)
                gotoNode = gobj.Notice.WhichNodes[receiverIndex].Name;

			var agentsWithJob = gobj.Notice.GetAgentsOnJob ().Select (a => new IilIdentifier (a.Name)).ToArray();



            ipc = new IilPerceptCollection
            (
                new IilPercept("receivedJob"),
                new IilPercept("noticeId", new IilNumeral(gobj.Notice.Id)),
		      	//new IilPercept("whichNodeNameToGoTo", new IilIdentifier(gotoNode))
		      	new IilPercept("agentsThatGotJob",agentsWithJob)
				//percept
            );

            return ipc;
        }
    }
}
