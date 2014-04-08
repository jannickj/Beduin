using NabfProject.KnowledgeManagerModel;
using NabfProject.NoticeBoardModel;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace NabfProject.SimManager
{
    public class SimulationFactory
    {
        public SimulationData ContructSimulationData()
        {
            return new SimulationData() { KnowledgeManager = new KnowledgeManager(), NoticeBoard = new NoticeBoard() };
        }
    }
}
