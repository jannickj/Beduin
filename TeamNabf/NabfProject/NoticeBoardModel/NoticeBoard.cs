using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JSLibrary.Data;
using NabfProject.AI;
using NabfProject.Events;
using NabfProject.KnowledgeManagerModel;

namespace NabfProject.NoticeBoardModel
{
    public class NoticeBoard
    {
        private DictionaryList<JobType, Notice> _availableJobs = new DictionaryList<JobType, Notice>();
        private SortedList<int, Notice[]> _jobs = new SortedList<int, Notice[]>(new InvertedComparer<int>());
        private Dictionary<Int64, Notice> _idToNotice = new Dictionary<Int64, Notice>();
        private DictionaryList<NabfAgent, Notice> _agentToNotice = new DictionaryList<NabfAgent, Notice>();
        private Int64 _freeID = 0;
        private HashSet<NabfAgent> _sharingList = new HashSet<NabfAgent>();

        public enum JobType { Empty = 0, Occupy = 1, Repair = 2, Disrupt = 3, Attack = 4 }
        public enum Status { available, unavailable}

        private const bool verbose = true;
        //status reporting for SimMan
        public int _agentsFiredCounter = 0;
        public int _nonUniqueJobsAttemptedToBeAdded = 0;
        public int _createdOccupyJob = 0;
        public int _createdAttackJob = 0;
        public int _createdRepairJob = 0;
        public int _createdDisruptJob = 0;
        
        public NoticeBoard()
        {
        }

        public bool TryGetNoticeFromId(Int64 noticeId, out Notice notice)
        {
            bool b = _idToNotice.TryGetValue(noticeId, out notice);
            if (b == false)
                return false;

            return true;
        }

        public bool Subscribe(NabfAgent agent)
        {
            if (_sharingList.Contains(agent))
                return false;
            _sharingList.Add(agent);
            return true;
        }
        public bool Unsubscribe(NabfAgent agent)
        {
            return _sharingList.Remove(agent);
        }

        public SortedList<int, Notice[]> GetJobs()
        {
            SortedList<int, Notice[]> newjobs = new SortedList<int, Notice[]>(new InvertedComparer<int>());
            foreach (KeyValuePair<int, Notice[]> kvp in _jobs)
                newjobs.Add(kvp.Key, kvp.Value);
            return newjobs;
        }
        public bool AddJob(Notice n)
        {
            if (n.HighestAverageDesirabilityForNotice == -1)
                return false;
            if (_jobs.ContainsKey(n.HighestAverageDesirabilityForNotice))
            {
                List<Notice> l = _jobs[n.HighestAverageDesirabilityForNotice].ToList();
                l.Add(n);
                _jobs.Remove(n.HighestAverageDesirabilityForNotice);
                _jobs.Add(n.HighestAverageDesirabilityForNotice, l.ToArray());
            }
            else
                _jobs.Add(n.HighestAverageDesirabilityForNotice, new Notice[] { n });

            return true;
        }
        public bool RemoveJob(Notice n)
        {
            if (_jobs.ContainsKey(n.HighestAverageDesirabilityForNotice))
            {
                List<Notice> l = _jobs[n.HighestAverageDesirabilityForNotice].ToList();
                l.Remove(n);
                bool b = _jobs.Remove(n.HighestAverageDesirabilityForNotice);
                if (l.Count == 0)
                    return b;
                if (b)
                {
                    _jobs.Add(n.HighestAverageDesirabilityForNotice, l.ToArray());
                    return true;
                }
                else
                    return false;
            }
            else
                return false;
        }

        private void ResetNoticeBoard()
        {
            _availableJobs.Clear();
            _jobs.Clear();
            _idToNotice.Clear();
            _agentToNotice.Clear();
            _freeID = 0;
        }

        public bool CreateAndAddNotice(JobType type, int agentsNeeded, List<NodeKnowledge> whichNodes, List<NodeKnowledge> zoneNodes, string agentToRepair, int value, out Notice notice)
        {
            Notice n = null;
            Int64 id = _freeID;
            _freeID++;
            switch (type)
            {
                case JobType.Attack:
                    n = new AttackJob(agentsNeeded, whichNodes, value, id);
                    break;
                case JobType.Disrupt:
                    n = new DisruptJob(agentsNeeded, whichNodes, value, id);
                    break;
                case JobType.Occupy:
                    n = new OccupyJob(agentsNeeded, whichNodes, zoneNodes, value, id);
                    break;
                case JobType.Repair:
                    n = new RepairJob(whichNodes, agentToRepair, value, id);
                    break;
            }
            if (n == null)
                throw new ArgumentException("Input to CreateNotice, JoType : " + type.GetType().Name + " was not of appropriate type. It's type was: " + type.GetType());
			bool isUnique = true;
			foreach (var job in _availableJobs)
			{
				foreach (Notice not in job.Value)
				{
					if (not.ContentIsEqualTo(n) || not.ContentIsSubsetOf(n))
						isUnique = false;
				}
			}

			notice = n;

            if (!isUnique)
            {
                _nonUniqueJobsAttemptedToBeAdded++;
                //if (verbose && _nonUniqueJobsAttemptedToBeAdded % 2 == 0)
                    //Console.WriteLine("Total number of received non-unique jobs: " + _nonUniqueJobsAttemptedToBeAdded);
                return false;
            }

            bool b = AddNotice(n);

            foreach (NabfAgent a in _sharingList)
            {
                a.Raise(new NewNoticeEvent(n));
                if (n is OccupyJob)
                    Console.WriteLine("WhichNode: " + ((OccupyJob)n).WhichNodes + ". ZoneNodes: " + ((OccupyJob)n).ZoneNodes);
                    //_createdOccupyJob++;
                if (n is RepairJob)
                    _createdRepairJob++;
                if (n is DisruptJob)
                    _createdDisruptJob++;
                if (n is AttackJob)
                    _createdAttackJob++;
            }

            return b;
        }

        private bool AddNotice(Notice no)
        {
            if (AvailableJobsContainsContentEqual(no))
                return false;
            _idToNotice.Add(no.Id, no);
            _availableJobs.Add(NoticeToJobType(no), no);
            return true;
        }

        public int GetNoticeCount()
        {
            return _availableJobs.TotalCount;
        }

        public int GetNoticeCount(Notice ofType)
        {
            return _availableJobs.Get(NoticeToJobType(ofType)).Count;
        }

        public int GetNoticeCount(JobType ofType)
        {
            return _availableJobs.Get(ofType).Count;
        }

        public bool RemoveNotice(Int64 id)
        {
            Notice notice;
            bool b = _idToNotice.TryGetValue(id, out notice);
            if (b == false)
                return false;
            return RemoveNotice(notice);
        }
        public bool RemoveNotice(Notice no)
        {
            if (!AvailableJobsContainsContentEqual(no))
                return false;

            foreach (NabfProject.AI.NabfAgent a in no.GetAgentsApplied())
                UnApplyToNotice(no, a, true);
            _idToNotice.Remove(no.Id);
            _availableJobs.Remove(NoticeToJobType(no), no);

            foreach (NabfAgent a in _sharingList)
                a.Raise(new NoticeRemovedEvent(no));

            return true;
        }

        public bool UpdateNotice(Int64 id, List<NodeKnowledge> whichNodes, List<NodeKnowledge> zoneNodes, int agentsNeeded, int value, string agentToRepair)
        {
            Notice no;
            bool b = _idToNotice.TryGetValue(id, out no);

            if (b == false)
                return false;

            no.UpdateNotice(whichNodes, zoneNodes, agentsNeeded, value, agentToRepair);

            foreach (NabfAgent a in _sharingList)
                a.Raise(new NoticeUpdatedEvent(id, no));

            return true;
        }

        public static JobType NoticeToJobType(Notice no)
        {
            if (no == null)
                throw new ArgumentNullException("Input to method NoticeToType was null.");

            if (no is DisruptJob)
                return JobType.Disrupt;
            else if (no is AttackJob)
                return JobType.Attack;
            else if (no is OccupyJob)
                return JobType.Occupy;
            else if (no is RepairJob)
                return JobType.Repair;
            else if (no is EmptyJob)
                return JobType.Empty;
            else
                throw new ArgumentException("Input to NoticeToJobtype, object : " + no.GetType().Name + " was not of appropriate type. It's type was: " + no.GetType());
        }

        public ICollection<Notice> GetNotices(List<JobType> jobs)
        {
            List<Notice> result = new List<Notice>();
            foreach(JobType jb in jobs)
            {
                result.AddRange(_availableJobs.Get(jb));
            }
            return result;
        }

        public ICollection<Notice> GetNotices()
        {
            List<JobType> jobs = new List<JobType>() { JobType.Attack, JobType.Disrupt, JobType.Occupy, JobType.Repair };
            List<Notice> result = new List<Notice>();
            foreach (JobType jb in jobs)
            {
                result.AddRange(_availableJobs.Get(jb));
            }
            return result;
        }

        public ICollection<Notice> GetNotices(NabfAgent agent)
        {
            throw new NotImplementedException();
        }

        public ICollection<Notice> GetUnavailableNotices(JobType type)
        {
            List<Notice> result = new List<Notice>();
            ICollection<Notice> noticesOfType = GetNotices(new List<NoticeBoard.JobType>() { type });
            foreach (Notice n in noticesOfType)
            {
                if (n.Status == Status.unavailable)
                    result.Add(n);
            }
            return result;
        }

        public void ApplyToNotice(Notice notice, int desirability, NabfAgent a)
        {
            foreach (Notice n in _availableJobs.Get(NoticeToJobType(notice)))
            {
                if (n.ContentIsEqualTo(notice))
                {
                    n.Apply(desirability, a);
                    _agentToNotice.Add(a, n);
                    break;
                }
            }
        }
        public void UnApplyToNotice(Notice notice, NabfAgent a, bool fireAll)
        {
            foreach (Notice n in _availableJobs.Get(NoticeToJobType(notice)))
            {
                if (n.ContentIsEqualTo(notice))
                {
                    n.UnApply(a, this);
                    _agentToNotice.Remove(a, n);
                    
                    //loop through each agent and fire them.
                    if (fireAll)
                    {
                        foreach (NabfAgent agent in notice.GetTopDesireAgents())
                        {
                            UnApplyToNotice(n, agent, false);
                            RaiseFiredEventForNotice(n, agent);
                            _agentsFiredCounter++;
                            if (verbose)
                            {
                                if (_agentsFiredCounter % 50 == 0)
                                    Console.WriteLine("Total number of fired agents: " + _agentsFiredCounter);
                            }
                        }
                    }

                    n.Status = Status.available;
                    break;
                }
            }
        }

        //public void UnApplyFromAll(NabfAgent a)
        //{
        //    foreach (Notice n in _agentToNotice[a])
        //        UnApplyToNotice(n, a);
        //}

        public  bool TryFindTopDesiresForNotice(Notice n, out int averageDesire, out List<NabfAgent> agents)
        {
			var applied = n.GetAgentsApplied();

			if (applied.Count >= n.AgentsNeeded)
			{

				Func<NabfAgent, int> desireOfAgent = an =>
					{
						int desire;
						n.TryGetValueAgentToDesirabilityMap(an, out desire);
						return desire;
					};

				var sortedagents = new SortedList<int, List<NabfAgent>>();

				Action<NabfAgent> addAgent = na =>
						{
							int desire = desireOfAgent(na);
							List<NabfAgent> desiredAgents;
							if(sortedagents.ContainsKey(desire))
								desiredAgents = sortedagents[desire];
							else
							{
								desiredAgents = new List<NabfAgent>();
								sortedagents[desire] = desiredAgents;
							}
							desiredAgents.Add(na);
						};
				foreach(var agent in applied)
				{
					addAgent(agent);
				}

				agents = sortedagents.Reverse().SelectMany(kv => kv.Value).Take(n.AgentsNeeded).ToList();
				averageDesire = agents.Sum(na => desireOfAgent(na)) / n.AgentsNeeded;
				return true;
			}
			agents = null;
			averageDesire = 0;
			return false;
        }

        public void FindJobsForAgents()
        {
            SortedList<int, Notice[]> jobs = PrepareSortedQueue();
            _jobs = new SortedList<int, Notice[]>(new InvertedComparer<int>());
            for (int i = jobs.Count - 1; i > -1; i--)
            {
                _jobs.Add(jobs.Keys[i], jobs.Values[i]);
            }

            List<NabfAgent> agentsWhoReceivedJob = new List<NabfAgent>();

            Notice notice;
            bool QueueNotEmpty = true;
            while (QueueNotEmpty)
            {
                notice = PopFromJobsList();
                QueueNotEmpty = (notice != null);
                if (notice != null && notice.AgentsNeeded <= notice.GetTopDesireAgents().Count && notice.Status == Status.available)
                {
					agentsWhoReceivedJob.AddRange(notice.GetTopDesireAgents());
                    RaiseEventForNotice(notice, false);
                    notice.Status = Status.unavailable;
                }
            }
//            Notice n;
//            foreach (NabfAgent agent in _sharingList.Except(agentsWhoReceivedJob))
//            {
//                n = new EmptyJob();
//                n.AddToTopDesireAgents(agent);
//                RaiseEventForNotice(n, false);
//            }
        }

        private Notice PopFromJobsList()
        {
            if (_jobs.Count == 0)
                return null;
            Notice n = _jobs.First().Value.First();
            if (_jobs.First().Value.Count() == 1)
                _jobs.RemoveAt(0);
            else
            {
                KeyValuePair<int, Notice[]> kv = _jobs.First();
                _jobs.Remove(kv.Key);
                _jobs.Add(kv.Key, kv.Value.Skip(1).ToArray());
            }
            return n;
        }
        
        private bool AvailableJobsContainsContentEqual(Notice no)
        {
            foreach (Notice n in _availableJobs.Get(NoticeToJobType(no)))
            {
                if (n.ContentIsEqualTo(no))
                    return true;
            }
            return false;
        }

        private SortedList<int, Notice[]> PrepareSortedQueue()
        {
            DictionaryList<int, Notice> dl = new DictionaryList<int, Notice>();
            List<NabfAgent> agents;
			bool success;
			int avg;

            foreach (Notice n in _availableJobs.SelectMany(kvp => kvp.Value))
            {
                success = TryFindTopDesiresForNotice(n, out avg, out agents);
                if (success)
                {
					n.HighestAverageDesirabilityForNotice = avg;
                    dl.Add(n.HighestAverageDesirabilityForNotice, n);
                    n.AddRangeToTopDesireAgents(agents);
                }
            }
            SortedList<int, Notice[]> jobs = new SortedList<int, Notice[]>(new InvertedComparer<int>());

            foreach (KeyValuePair<int, Notice[]> kvp in dl)
                jobs.Add(kvp.Key, kvp.Value);

            return jobs;
        }

        private bool RaiseEventForNotice(Notice n, bool fireOtherAtEnd) 
        {
            //NoticeIsReadyToBeExecutedEventArgs args = new NoticeIsReadyToBeExecutedEventArgs();
            //args.Agents = n.GetTopDesireAgents();
            //args.Notice = n;

            //if (NoticeIsReadyToBeExecutedEvent != null)
            //    NoticeIsReadyToBeExecutedEvent(this, args);
            foreach (NabfAgent a in n.GetTopDesireAgents())
            {
                a.Raise(new ReceivedJobEvent(n, a));
                foreach (Notice no in _agentToNotice[a])
                {
                    if (no.ContentIsEqualTo(n))
                        continue;
                    Console.WriteLine("Agent " + a.Name + " unapplied from " + no.ToString() + " in favor of " + n.ToString());
                    UnApplyToNotice(no, a, fireOtherAtEnd);
                }
            }
            return true;
        }

        private bool RaiseFiredEventForNotice(Notice n, NabfAgent a)
        {
            a.Raise(new FiredFromJobEvent(n, a));

            return true;
        }

        public int GetSubscribedAgentsCount()
        {
            return _sharingList.Count;
        }
        public ICollection<NabfAgent> GetSubscribedAgents()
        {
            return _sharingList.ToList();
        }

        public bool AgentIsSubscribed(NabfAgent agent)
        {
            return _sharingList.Contains(agent);
        }

        public void SendOutAllNoticesToAgent(NabfAgent agent)
        {
            foreach (KeyValuePair<JobType, Notice[]> kvp in _availableJobs)
            {
                foreach (Notice n in kvp.Value)
                {
                    if (n.Status == Status.available)
                        agent.Raise(new NewNoticeEvent(n));
                }
            }
        }
    }

    
}
