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

        private const bool verbose = false;
        private const bool reporting = true;
        //Status reporting
        private int _callsToSendKnowledge = 0;
        private int _sentJobCounter = 0;
        private int _updatedJobCounter = 0;
        private int _applicationsReceivedCounter = 0;
        private int _unappliesReceivedCounter = 0;
        private int _noticesRemovedCounter = 0;
        private int _sentKnowledgeCounter = 0;
        private bool _notPrintedSentKnowledgeCounterThisRound = true;
        private int _sentOccupyJobCounter = 0;
        private int _sentRepairJobCounter = 0;
        private int _sentDisruptJobCounter = 0;
        private int _sentAttackJobCounter = 0;

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

            _sentKnowledgeCounter += sentKnowledge.Count;
            if (verbose)
            {
                if (_currentRoundNumber % 20 == 0 && _notPrintedSentKnowledgeCounterThisRound)
                {
                    Console.WriteLine("Total numbers of sent knowledge is: " + _sentKnowledgeCounter);
                    _notPrintedSentKnowledgeCounterThisRound = false;
                }
                else if (_currentRoundNumber % 9 == 0)
                    _notPrintedSentKnowledgeCounterThisRound = true;
            }
        }

        public bool CreateAndAddNotice(int simID, NoticeBoard.JobType type, int agentsNeeded, List<NodeKnowledge> whichNodes, List<NodeKnowledge> zoneNodes, string agentToRepair, int value, out Notice notice)
        {
            NoticeBoard nb;
            TryGetNoticeBoard(simID, out nb);

            bool ret = nb.CreateAndAddNotice(type, agentsNeeded, whichNodes, zoneNodes, agentToRepair, value, out notice);

            _sentJobCounter++;
            if (verbose)
            {
                if (_sentJobCounter % 10 == 0)
                    Console.WriteLine("Total numbers of created jobs is: " + _sentJobCounter);
            }

            #region status report data gathering
            if (type == NoticeBoard.JobType.Attack)
                _sentAttackJobCounter++;
            if (type == NoticeBoard.JobType.Repair)
                _sentRepairJobCounter++;
            if (type == NoticeBoard.JobType.Occupy)
                _sentOccupyJobCounter++;
            if (type == NoticeBoard.JobType.Disrupt)
                _sentDisruptJobCounter++;
            #endregion

            return ret;
        }

        public bool RemoveNotice(int simID, Int64 noticeId)
        {
            if (_currentID != simID)
                return false;
            NoticeBoard nb;
            TryGetNoticeBoard(simID, out nb);

            _noticesRemovedCounter++;
            if (verbose)
            {
                if (_noticesRemovedCounter % 10 == 0)
                    Console.WriteLine("Total numbers of jobs removed is: " + _noticesRemovedCounter);
            }

            return nb.RemoveNotice(noticeId);
        }

        public bool UpdateNotice(int simID, Int64 noticeID, int agentsNeeded, List<NodeKnowledge> whichNodes, List<NodeKnowledge> zoneNodes, string agentToRepair, int value)
        {
            if (_currentID != simID)
                return false;
            NoticeBoard nb;
            TryGetNoticeBoard(simID, out nb);

            _updatedJobCounter++;
            if (verbose)
            {
                if (_updatedJobCounter % 10 == 0)
                    Console.WriteLine("Total numbers of updated jobs is: " + _updatedJobCounter);
            }

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

            if (_numberOfAgentsFinishedApplying >= numberOfAgents)
                FindJobs(simID);

            _applicationsReceivedCounter++;
            if (verbose)
            {
                if (_applicationsReceivedCounter % 100 == 0)
                    Console.WriteLine("Total numbers of job applications received is: " + _applicationsReceivedCounter);
            }
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

            _unappliesReceivedCounter++;
            if (verbose)
            {
                if (_unappliesReceivedCounter % 100 == 0)
                    Console.WriteLine("Total numbers of job un-applications received is: " + _unappliesReceivedCounter);
            }

            Console.WriteLine("Agent " + a.Name + " unapplied from " + notice.ToString());
            //return; //disabling unapply
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

            //by un-commenting this code, agents no longer unapplies from all jobs when starting a new round
            //foreach(NabfAgent a in nb.GetSubscribedAgents())
            //    nb.UnApplyFromAll(a);

            try { this.EventManager.Raise(new RoundChangedEvent(_currentRoundNumber)); }
            catch { }
            _applicationClosed = false;
            _jobsFoundForThisRound = false;
            _numberOfAgentsFinishedApplying = 0;

            //if (_currentRoundNumber % 5 == 0 || _currentRoundNumber < 10)
                Console.WriteLine("-------- Simulation " + simID + ", Round: " + _currentRoundNumber + " --------");

            if (reporting)
            {
                #region status reports
                if (_currentRoundNumber % 100 == 0 && _currentRoundNumber > 1)
                {
                    KnowledgeManager km;
                    TryGetKnowledgeManager(simID, out km);
                    Console.WriteLine("  ");
                    Console.WriteLine("--- status on all current jobs on round " + _currentRoundNumber + " ---");
                    Console.WriteLine("Total number of sent jobs: " + _sentJobCounter);
                    Console.WriteLine("      Total sent occupy jobs: " + _sentOccupyJobCounter + ". Occupy jobs currently available: " + nb.GetNotices(NoticeBoard.JobType.Occupy).Count() + ". Occupy jobs currently in use: " + nb.GetUnavailableNotices(NoticeBoard.JobType.Occupy).Count());
                    Console.WriteLine("      Total sent repair jobs: " + _sentRepairJobCounter + ". Repair jobs currently available: " + nb.GetNotices(NoticeBoard.JobType.Repair).Count() + ". Repair jobs currently in use: " + nb.GetUnavailableNotices(NoticeBoard.JobType.Repair).Count());
                    Console.WriteLine("      Total sent attack jobs: " + _sentAttackJobCounter + ". Attack jobs currently available: " + nb.GetNotices(NoticeBoard.JobType.Attack).Count() + ". Attack jobs currently in use: " + nb.GetUnavailableNotices(NoticeBoard.JobType.Attack).Count());
                    Console.WriteLine("      Total sent disrupt jobs: " + _sentDisruptJobCounter + ". Disrupt jobs currently available: " + nb.GetNotices(NoticeBoard.JobType.Disrupt).Count() + ". Disrupt jobs currently in use: " + nb.GetUnavailableNotices(NoticeBoard.JobType.Disrupt).Count());
                    Console.WriteLine("Total number of sent job updates: " + _updatedJobCounter);
                    Console.WriteLine("Total number of sent applications: " + _applicationsReceivedCounter);
                    Console.WriteLine("Total number of sent un-applications: " + _unappliesReceivedCounter);
                    Console.WriteLine("Total number of agents fired: " + nb._agentsFiredCounter);
                    Console.WriteLine("Total number of sent job removals: " + _noticesRemovedCounter);
                    Console.WriteLine("Total number of received non-unique jobs: " + nb._nonUniqueJobsAttemptedToBeAdded);

                    if (nb.GetSubscribedAgentsCount() < 28)
                        Console.WriteLine("WARNING! there is only " + nb.GetSubscribedAgentsCount() + " agents connected to Notice Board");

                    Console.WriteLine("  ");

                    Console.WriteLine("--- status on all current knowledge on round " + _currentRoundNumber + " ---");
                    Console.WriteLine("Knowledge base size: " + km.KnowledgeBase.Length);
                    Console.WriteLine("Total knowledge sent: " + _sentKnowledgeCounter);
                    Console.WriteLine("Total redudant knowledge sent: " + (km._redudantEdgeKnowledgeCounter + km._redudantNodeKnowledgeCounter + km._redudantRoleKnowledgeCounter + km._redudantHeuristicKnowledgeCounter + km._redudantMessageKnowledgeCounter));
                    Console.WriteLine("Total useful knowledge sent: " + (km._edgeKnowledgeCounter + km._nodeKnowledgeCounter + km._roleKnowledgeCounter + km._heuristicKnowledgeCounter + km._messageKnowledgeCounter));
                    Console.WriteLine("Redudant knowledge sent. Edge: " + km._redudantEdgeKnowledgeCounter + ". Node " + km._redudantNodeKnowledgeCounter + ". Role " + km._redudantRoleKnowledgeCounter + ". Message " + km._redudantMessageKnowledgeCounter + ". Heuristic " + km._redudantHeuristicKnowledgeCounter);
                    Console.WriteLine("Useful knowledge sent. Edge: " + km._edgeKnowledgeCounter + ". Node " + km._nodeKnowledgeCounter + ". Role " + km._roleKnowledgeCounter + ". Message " + km._messageKnowledgeCounter + ". Heuristic " + km._heuristicKnowledgeCounter);

                    if (km.GetSubscribedAgentsCount() < 28)
                        Console.WriteLine("WARNING! there is only " + km.GetSubscribedAgentsCount() + " agents connected to Knowledge Manager");
                    Console.WriteLine("  ");
                }
                #endregion
            }
            //Console.WriteLine("Number of occupy jobs: "+nb.GetNoticeCount(NoticeBoard.JobType.Occupy));
            //Console.WriteLine("Number of taken occupy jobs: " + nb.GetUnavailableNotices(NoticeBoard.JobType.Occupy).Count);
            //Console.WriteLine("Number of fired agents: " + nb._agentsFiredCounter);
            //Console.WriteLine("Total number of agent applications on non-taken jobs: " + nb.CountNumberOfApplications(nb.GetAvailableNotices(NoticeBoard.JobType.Occupy)));
            foreach (Notice n in nb.GetAvailableJobs(NoticeBoard.JobType.Occupy))
            {
                //foreach (NabfAgent a in n.GetTopDesireAgents())
                //    Console.WriteLine("" + a.Name + " has " + n.ToString());
                //foreach (NabfAgent a in n.GetAgentsApplied())
                //    Console.WriteLine(""+a.Name);
                
            }
            
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
