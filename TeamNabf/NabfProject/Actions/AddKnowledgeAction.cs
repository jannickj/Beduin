using NabfProject.AI;
using NabfProject.SimManager;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using XmasEngineModel.EntityLib;
using XmasEngineModel.Management;
using NabfProject.KnowledgeManagerModel;

namespace NabfProject.Actions
{
    public class AddKnowledgeAction : EntityXmasAction<NabfAgent>
    {
        private int SimId;
        private List<Knowledge> SentKnowledge;

        public AddKnowledgeAction(int simID, List<Knowledge> sentKnowledge)
        {
            SimId = simID;
            SentKnowledge = sentKnowledge;
        }

        protected override void Execute()
        {
            SimulationManager simMan = ((NabfModel)this.Engine).SimulationManager;

            simMan.SendKnowledge(SimId, SentKnowledge, this.Source);
        }
    }
}
