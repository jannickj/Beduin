using NabfProject.AI;
using NabfProject.SimManager;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using XmasEngineModel.EntityLib;
using XmasEngineModel.Management;
using NabfProject.KnowledgeManagerModel;
using NabfProject.NoticeBoardModel;

namespace NabfProject.Actions
{
    public class CreateNoticeAction : EntityXmasAction<NabfAgent>
    {
        private int SimId;
        private NoticeBoardModel.NoticeBoard.JobType JobType;
        private int AgentsNeeded;
        private List<NodeKnowledge> WhichNodes;
        private int Value;
        private string AgentToRepair;
        private List<NodeKnowledge> ZoneNodes;

        public CreateNoticeAction(int simID, NoticeBoard.JobType jobType, int agentsNeeded, List<NodeKnowledge> whichNodes, List<NodeKnowledge> zoneNodes, string agentToRepair, int value)
        {
            SimId = simID;
            JobType = jobType;
            AgentsNeeded = agentsNeeded;
            WhichNodes = whichNodes;
            ZoneNodes = zoneNodes;
            AgentToRepair = agentToRepair;
            Value = value;
        }
        protected override void Execute()
        {
            SimulationManager simMan = ((NabfModel)this.Engine).SimulationManager;

			

            Notice n;
            simMan.CreateAndAddNotice(SimId, JobType, AgentsNeeded, WhichNodes, ZoneNodes, AgentToRepair, Value, out n);
			//string nodes = n.WhichNodes.Select(nk => nk.ToString() + ", ").Aggregate((i, j) => i + j);
			//Console.WriteLine("Added job: " + n + " with nodes: " + nodes);
        }
    }
}
