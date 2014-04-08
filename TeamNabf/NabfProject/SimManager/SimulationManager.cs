using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NabfProject.KnowledgeManagerModel;
using NabfProject.NoticeBoardModel;
using NabfProject.AI;
using NabfProject.Events;
using XmasEngineModel.EntityLib;

namespace NabfProject.SimManager
{
    public class SimulationManager : XmasUniversal
    {
		public int CurrentRoundNumber { get { return _currentRoundNumber; } }

        public int TimeBeforeApplyCloses { get; private set; }
        private const int _standardTimeBeforeApplyCloses = 1000;

        private Dictionary<int, SimulationData> _simDataStorage = new Dictionary<int, SimulationData>();
        private SimulationFactory _factory;
        private int _currentID = -1;
		private int _currentRoundNumber = -1;

		
        private bool _applicationClosed = false;
        private bool _jobsFoundForThisRound = false;
        private int _numberOfAgentsFinishedApplying = 0;

        public SimulationManager(SimulationFactory sf, int timeBeforeApplyCloses = _standardTimeBeforeApplyCloses)
        {
            TimeBeforeApplyCloses = timeBeforeApplyCloses;
            _factory = sf;
        }

        private bool TryGetNoticeBoard(int simID, out NoticeBoard nb)
        {
            KnowledgeManager km;
            TryGetSimData(simID, out km, out nb);
            return true;
        }
        private bool TryGetKnowledgeManager(int simID, out KnowledgeManager km)
        {
            NoticeBoard nb;
            TryGetSimData(simID, out km, out nb);
            return true;
        }
        private bool TryGetSimData(int simID, out KnowledgeManager km, out NoticeBoard nb)
        {
            bool b = false;
            SimulationData sd;
            km = null;
            nb = null;

            b = _simDataStorage.TryGetValue(simID, out sd);
            if (b == false)
                return false;// throw new ArgumentException("id " + simID + " not found.");
                //return false;

            km = sd.KnowledgeManager;
            nb = sd.NoticeBoard;

            return b;
        }

        private bool TryInsertSimData(int simID)
        {
            bool b = true;

            try
            {
                _simDataStorage.Add(simID, _factory.ContructSimulationData());
            }
            catch
            {
                b = false;
            }

            return b;
        }

        public void SubscribeToSimulation(int simID, NabfAgent agent)
        {
            KnowledgeManager km;
            NoticeBoard nb;
            bool b;
            if (_currentID != simID)
            {
                b = TryGetSimData(_currentID, out km, out nb);
                if (b)
                {
                    km.Unsubscribe(agent);
                    nb.Unsubscribe(agent);
                }

                TryInsertSimData(simID);
                _currentID = simID;
            }

            TryGetSimData(simID, out km, out nb);
            if (!nb.AgentIsSubscribed(agent))
            {
                km.Subscribe(agent);
                nb.Subscribe(agent);
                //agent.Raise(new SimulationSubscribedEvent(simID));
            }

            //send out all knowledge to agent
            agent.Raise(new SimulationSubscribedEvent(simID));
            try { this.EventManager.Raise(new RoundChangedEvent(_currentRoundNumber)); }
            catch { }
            km.SendOutAllKnowledgeToAgent(agent);
            nb.SendOutAllNoticesToAgent(agent);
        }

        public void SendKnowledge(int id, List<Knowledge> sentKnowledge, NabfAgent sender)
        {
            if (id != _currentID)
                return;

            KnowledgeManager km;
            TryGetKnowledgeManager(id, out km);

            km.SendKnowledgeToManager(sentKnowledge, sender);
        }

        public bool CreateAndAddNotice(int simID, NoticeBoard.JobType type, int agentsNeeded, List<NodeKnowledge> whichNodes, List<NodeKnowledge> zoneNodes, string agentToRepair, int value, out Notice notice)
        {
            NoticeBoard nb;
            TryGetNoticeBoard(simID, out nb);

            bool ret = nb.CreateAndAddNotice(type, agentsNeeded, whichNodes, zoneNodes, agentToRepair, value, out notice);

            return ret;
        }

        public bool RemoveNotice(int simID, Int64 noticeId)
        {
            if (_currentID != simID)
                return false;
            NoticeBoard nb;
            TryGetNoticeBoard(simID, out nb);

            return nb.RemoveNotice(noticeId);
        }

        public bool UpdateNotice(int simID, Int64 noticeID, int agentsNeeded, List<NodeKnowledge> whichNodes, List<NodeKnowledge> zoneNodes, string agentToRepair, int value)
        {
            if (_currentID != simID)
                return false;
            NoticeBoard nb;
            TryGetNoticeBoard(simID, out nb);

            return nb.UpdateNotice(noticeID, whichNodes, zoneNodes, agentsNeeded, value, agentToRepair);               
        }

        public NoticeBoard.JobType NoticeToJobType(Notice no)
        {
            NoticeBoard nb;
            TryGetNoticeBoard(_currentID, out nb);

            return NoticeBoard.NoticeToJobType(no);
        }

        public void ApplyToNotice(int simID, Int64 noticeId, int desirability, NabfAgent a)
        {
            if (_currentID != simID || _applicationClosed)
                return;
            NoticeBoard nb;
            Notice notice;
            TryGetNoticeBoard(simID, out nb);
            if (noticeId != -1)
            {
                bool b = nb.TryGetNoticeFromId(noticeId, out notice);
                if (b == false)
                    return;
            }
            else
                notice = new EmptyJob();

            if (notice.IsEmpty())
                _numberOfAgentsFinishedApplying++;
            else
                nb.ApplyToNotice(notice, desirability, a);

            int numberOfAgents = nb.GetSubscribedAgentsCount();            

            if (numberOfAgents <= _numberOfAgentsFinishedApplying)
                FindJobs(simID);
        }
        public void UnApplyToNotice(int simID, Int64 noticeId, NabfAgent a)
        {
            if (_currentID != simID)
                return;

            NoticeBoard nb;
            Notice notice;
            TryGetNoticeBoard(simID, out nb);
            bool b = nb.TryGetNoticeFromId(noticeId, out notice);
            if (b == false)
                return;

            nb.UnApplyToNotice(notice, a, true);
        }

        private void FindJobsForAgents(int simID)
        {
            if (_currentID != simID)
                return;
            NoticeBoard nb;
            TryGetNoticeBoard(simID, out nb);

            nb.FindJobsForAgents();
        }

        public bool TryGoNextRound(int simID, int roundNumber)
        {
            if (_currentID != simID || roundNumber <= _currentRoundNumber)
                return false;

            _currentRoundNumber++;
            NoticeBoard nb;
            TryGetNoticeBoard(simID, out nb);

            //agents no longer unapplies from all jobs when starting a new round
            //foreach(NabfAgent a in nb.GetSubscribedAgents())
            //    nb.UnApplyFromAll(a);

            try { this.EventManager.Raise(new RoundChangedEvent(_currentRoundNumber)); }
            catch { }
            _applicationClosed = false;
            _jobsFoundForThisRound = false;
            _numberOfAgentsFinishedApplying = 0;
            return true;
        }

        public void FindJobs(int simID)
        {
            if (_jobsFoundForThisRound == false) 
            {
                _jobsFoundForThisRound = true;
                _applicationClosed = true;
                FindJobsForAgents(simID);
            }
        }
    }

    public struct SimulationData
    {
        public KnowledgeManager KnowledgeManager { get; set; }
        public NoticeBoard NoticeBoard { get; set; }
    }
}
