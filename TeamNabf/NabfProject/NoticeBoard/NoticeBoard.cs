using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NabfProject.AI;
using NabfProject.Events;
using NabfProject.KnowledgeManagerModel;
using JSLibrary;
using JSLibrary.Data;

namespace NabfProject.NoticeBoardModel
{
    public class NoticeBoard : NoticeBoardHelpers
    {
        private Int64 _freeID = 0;
        private HashSet<NabfAgent> _sharingList = new HashSet<NabfAgent>();
        private Dictionary<Int64, Notice> _allNotices = new Dictionary<Int64, Notice>();
        private DictionaryList<string, Int64> _agentToAppliedNotices = new DictionaryList<string, Int64>();

        public enum JobType { Empty = 0, Occupy = 1, Repair = 2, Disrupt = 3, Attack = 4 }
        public enum Status { available, unavailable }

        private const bool verbose = true;
        //status reporting for SimMan
        public int _agentsFiredCounter = 0;
        public int _nonUniqueJobsAttemptedToBeAdded = 0;
        public int _createdOccupyJob = 0;
        public int _createdAttackJob = 0;
        public int _createdRepairJob = 0;
        public int _createdDisruptJob = 0;
        public List<Notice> _nonUniqueJobs = new List<Notice>();



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
        public bool AgentIsSubscribed(NabfAgent agent)
        {
            return _sharingList.Contains(agent);
        }

        public void AddNoticeToAllNotices(Notice n)
        {
            _allNotices.Add(n.Id, n);
        }                

        public bool CreateNotice(JobType jobType, int agentsNeeded, List<NodeKnowledge> whichNodesIsInvolvedInJob, List<NodeKnowledge> whichNodesToStandOn, string agentToRepair, int timeStamp, int jobValue)
        {
            Notice n = null;
            Int64 id = _freeID;
            _freeID++;
            switch (jobType)
            {
                case JobType.Attack:
                    n = new AttackJob(agentsNeeded, whichNodesIsInvolvedInJob, timeStamp, jobValue, id);
                    break;
                case JobType.Disrupt:
                    n = new DisruptJob(agentsNeeded, whichNodesIsInvolvedInJob, jobValue, id);
                    break;
                case JobType.Occupy:
                    n = new OccupyJob(agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, jobValue, id);
                    break;
                case JobType.Repair:
                    n = new RepairJob(whichNodesIsInvolvedInJob, agentToRepair, jobValue, id);
                    break;
            }
            if (n == null)
                throw new ArgumentException("Input to CreateNotice, JoType : " + jobType.GetType().Name + " was not of appropriate type. It's type was: " + jobType.GetType());            

            return AddNotice(n);
        }
        private bool AddNotice(Notice n)
        {
            bool isUnique = true;
            foreach (Notice job in GetAllNotices())
            {
                if (job.ContentIsEqualTo(n) || job.ContentIsSubsetOf(n) || job.Equals(n))
                    isUnique = false;
            }

            if (!isUnique)
            {
                _nonUniqueJobsAttemptedToBeAdded++;
                _nonUniqueJobs.Add(n);
                if (verbose && _nonUniqueJobsAttemptedToBeAdded % 2 == 0)
                    Console.WriteLine("Total number of received non-unique jobs: \n" + _nonUniqueJobsAttemptedToBeAdded);
                return false;
            }

            AddNoticeToAllNotices(n);

            foreach (NabfAgent a in _sharingList)
            {
                a.Raise(new NewNoticeEvent(n));
            }
            if (n is OccupyJob)
                _createdOccupyJob++;
            if (n is RepairJob)
                _createdRepairJob++;
            if (n is DisruptJob)
                _createdDisruptJob++;
            if (n is AttackJob)
                _createdAttackJob++;

            if (verbose)
                Console.WriteLine("An agent posted " + n.ToString());
            return true;
        }
        public bool UpdateNotice(Int64 id, List<NodeKnowledge> whichNodesIsInvolvedInJob, List<NodeKnowledge> whichNodesToStandOn, int agentsNeeded, int jobValue, string agentToRepair)
        {
            Notice no;
            bool b = _allNotices.TryGetValue(id, out no);

            if (b == false)
                return false;

            no.UpdateNotice(whichNodesIsInvolvedInJob, agentsNeeded, jobValue, whichNodesToStandOn, agentToRepair);

            foreach (NabfAgent a in _sharingList)
            {
                a.Raise(new NoticeUpdatedEvent(id, no));
            }

            return true;
        }
        public bool DeleteNotice(Int64 id)
        {
            Notice notice;
            bool b = _allNotices.TryGetValue(id, out notice);
            if (b == false)
                return false;

            foreach (NabfAgent a in notice.GetAgentsOnJob())
            {
                a.Raise(new FiredFromJobEvent(notice, a));
                _agentsFiredCounter++;
                if (verbose)
                {
                    if (_agentsFiredCounter % 20 == 0)
                        Console.WriteLine("Total number of fired agents: " + _agentsFiredCounter);
                }
            }
            _allNotices.Remove(notice.Id);

            foreach (NabfAgent a in _sharingList)
            {
                a.Raise(new NoticeRemovedEvent(notice));
            }

            return true;
        }

        /// <summary>
        /// Sends out all notices to an agent. 
        /// Only to be used when reconnecting after a disconnect.
        /// </summary>
        /// <param name="agent">Agent to send data to</param>
        public void SendOutAllNoticesToAgent(NabfAgent agent)
        {
            foreach (KeyValuePair<Int64, Notice> kvp in _allNotices)
            {
                agent.Raise(new NewNoticeEvent(kvp.Value));
            }
        }

        public bool ApplyToNotice(NabfAgent agent, Int64 idToApplyTo, double desireAppliedWith)
        {
            Notice noticeAppliedTo;
            bool b = TryGetNoticeById(idToApplyTo, out noticeAppliedTo); 
            if (b == false)
                return false;
            if (noticeAppliedTo.Status == Status.unavailable)
                return false;

            noticeAppliedTo.Apply(desireAppliedWith, agent);
            _agentToAppliedNotices.Add(agent.Name, noticeAppliedTo.Id);

            return true;
        }
        public bool UnapplyToNotice(NabfAgent agent, Int64 idToUnapplyTo)
        {
            Notice noticeUnappliedTo;
            bool b = TryGetNoticeById(idToUnapplyTo, out noticeUnappliedTo);
            if (b == false)
                return false;
            bool agentHasApplied = false;
            foreach (NabfAgent a in noticeUnappliedTo.GetAgentsApplied())
            {
                if (agent.Equals(a))
                {
                    agentHasApplied = true;
                    break;
                }
            }

            if (agentHasApplied == false)
                return false;

            bool agentHadJob = AgentListContainsAgent(noticeUnappliedTo.GetAgentsOnJob(), agent);

            _agentToAppliedNotices.Remove(agent.Name, noticeUnappliedTo.Id);

            //if agent has the job, fire the rest and set status to free
            if (agentHadJob)
            {
                FireOtherAgentsOnNotice(agent, idToUnapplyTo);
                noticeUnappliedTo.Status = Status.available;
                //noticeUnappliedTo.AvgDesirabilityAmongTopDesires = -1;
            }

            noticeUnappliedTo.Unapply(agent);

            return true;
        }
        public bool FireOtherAgentsOnNotice(NabfAgent issuedByAgent, Int64 idToUnapplyTo)
        {
            Notice noticeUnappliedTo;
            bool b = TryGetNoticeById(idToUnapplyTo, out noticeUnappliedTo);
            if (b == false)
                return false;

            foreach (NabfAgent a in noticeUnappliedTo.GetAgentsOnJob())
            {
                if (a.Equals(issuedByAgent))
                    continue;
                noticeUnappliedTo.Unapply(a);
                a.Raise(new FiredFromJobEvent(noticeUnappliedTo, a));

                _agentsFiredCounter++;
                if (verbose && _agentsFiredCounter % 20 == 0)
                    Console.WriteLine("Total number of fired agents: " + _agentsFiredCounter);
            }

            return true;
        }

        #region AssignJobs
        public bool AssignJobs()
        {
            Notice notice;

            foreach (NabfAgent a in _sharingList)
            {
                a.GotJobThisRound = false;
            }

            Queue<Notice> jobQueue = CreateQueueSortedByAvgDesirability();

            if (jobQueue.Count <= 0)
                return false;
            while(jobQueue.Count > 0)
            {
                notice = jobQueue.Dequeue();

                notice.Status = Status.unavailable;
                notice.AddRangeToAgentsOnJob(notice.GetAgentProspects());
                notice.ClearAgentProspects();
                foreach (NabfAgent agent in notice.GetAgentsOnJob())
                {
                    agent.GotJobThisRound = true;
                    agent.Raise(new ReceivedJobEvent(notice, agent));
                    Console.WriteLine(""+agent.Name+" got "+notice.ToString());

                    //foreach (Notice noticeToFireFrom in GetAllNotices())
                    //{
                    //    if (AgentListContainsAgent(noticeToFireFrom.GetAgentsOnJob(), agent) && noticeToFireFrom.Id != notice.Id)
                    //        FireAllAgentsOnNotice(noticeToFireFrom);
                    //}
                    jobQueue = CreateQueueSortedByAvgDesirability();
                }
            }
            
            HashSet<string> nameChecking = new HashSet<string>();
            bool b;
//            foreach (Notice n in GetAllNotices())
//            {
//                if (n.GetAgentsOnJob().Count > n.AgentsNeeded)
//                {
//                    Console.WriteLine("job with ID " + n.Id + " has more agents on job than agents needed");
//                    Console.WriteLine("job with ID " + n.Id + " has more agents on job than agents needed");
//                    Console.WriteLine("job with ID " + n.Id + " has more agents on job than agents needed");
//                    Console.WriteLine("job with ID " + n.Id + " has more agents on job than agents needed");
//                    Console.WriteLine("job with ID " + n.Id + " has more agents on job than agents needed");
//                }
//
//                foreach (NabfAgent a in n.GetAgentsOnJob())
//                {
//                    b = nameChecking.Add(a.Name);
//                    if (b == false)
//                    {
//                        Console.WriteLine("" + a.Name + " is in multiple OnJob lists");
//                    }
//                }
//            }
            foreach (Notice n in GetAllNotices())
            {
                n.ClearAgentsApplied();
            }
            return true;
        }
        private void FireAllAgentsOnNotice(Notice noticeToFireFrom)
        {
            foreach (NabfAgent a in noticeToFireFrom.GetAgentsOnJob())
            {
                a.Raise(new FiredFromJobEvent(noticeToFireFrom, a));
            }
            noticeToFireFrom.Status = Status.available;
            noticeToFireFrom.ClearAgentsOnJob();
        }

        //Creates a queue of jobs, sorted after average desirability, which has AgentsApplied >= AgentsNeeded. 
        //Average desirability is calculated here as:
        //the sum of the desire of the K agents with highest desire, divided by K
        //The method also puts the top K agents into AgentProspects
        public Queue<Notice> CreateQueueSortedByAvgDesirability()
        {
            Queue<Notice> jobQueue = new Queue<Notice>();
            List<NabfAgent> namesOfTopContenders;
            //SortedList<double, NewNotice> sortedNoticeList = new SortedList<double, NewNotice>(new InvertedComparer<double>());//the list now sorts descending
            List<KeyValuePair<double, Notice>> noticeList = new List<KeyValuePair<double, Notice>>();
            foreach (Notice n in GetAllNotices())
            {
                n.ClearAgentProspects();
            }
            

            foreach (Notice notice in GetAllNotices())
            {
                if (notice.EnoughAvailableAgentsApplied() && notice.Status == Status.available)
                {
                    noticeList.Add(new KeyValuePair<double, Notice>(CalculateAverageDesireForTopContenders(notice, out namesOfTopContenders), notice));
                    notice.AddRangeToAgentProspects(namesOfTopContenders);
                }
            }
		
            noticeList.Sort(new KvpKeyInvertedComparer<double, Notice>());

            for (int i = 0; i < noticeList.Count; i++)
            {
                jobQueue.Enqueue(noticeList[i].Value);
            }

            return jobQueue;
        }
        public double CalculateAverageDesireForTopContenders(Notice notice, out List<NabfAgent> namesOfTopContenders)
        {
            double result = -1.0;
            double desire;
            bool desireFound;
            namesOfTopContenders = new List<NabfAgent>();
            //SortedList<int, NabfAgent> sortedAgentList = new SortedList<int, NabfAgent>(new InvertedComparer<int>());//the list now sorts descending
            List<KeyValuePair<double, NabfAgent>> agentList = new List<KeyValuePair<double, NabfAgent>>();

            foreach (NabfAgent agent in notice.GetAgentsApplied())
            {
                if (agent.GotJobThisRound)
                    continue;
                desireFound = notice.TryGetDesirabilityOfAgent(agent, out desire);
                if (desireFound)
                {
                    agentList.Add(new KeyValuePair<double, NabfAgent>(desire, agent));
                }
            }
            agentList.Sort(new KvpKeyInvertedComparer<double, NabfAgent>());

            //calculate the sum of desire for the first K agents in sortedAgentList, where K is AgentsNeeded
            double desireSum = 0, agentsNeeded = notice.AgentsNeeded;
            for (int i = 0; i < notice.AgentsNeeded; i++)
            {
                desireSum += agentList[i].Key;
                namesOfTopContenders.Add((NabfAgent)agentList[i].Value);
            }

            result = (desireSum / agentsNeeded);
            notice.AverageDesireFromTopContenders = result;

            return result;
        }

        public void ConsistencyChecker()
        {
            return;
            foreach (KeyValuePair<Int64, Notice> kvp in _allNotices)
            {
                if (kvp.Value.Status == Status.unavailable)
                {
                    if (kvp.Value.GetAgentsOnJob().Count <= 0)
                        kvp.Value.Status = Status.available;
                }
            }
        }
        #endregion

        #region Get'ers
        public ICollection<NabfAgent> GetSubscribedAgents()
        {
            return _sharingList.ToList();
        }

        public ICollection<Notice> GetAllNotices()
        {
            return _allNotices.Values.ToList();
        }
        public bool TryGetNoticeById(Int64 id, out Notice notice)
        {
            bool b = _allNotices.TryGetValue(id, out notice);
            return b;
        }
        internal ICollection<Notice> GetUnavailableNotices()
        {
            List<Notice> result = new List<Notice>();

            foreach (KeyValuePair<Int64, Notice> kvp in _allNotices)
            {
                if (kvp.Value.Status == Status.unavailable)
                    result.Add(kvp.Value);
            }

            return result;
        }
        internal ICollection<Notice> GetUnavailableNotices(JobType type)
        {
            List<Notice> result = new List<Notice>();

            foreach (KeyValuePair<Int64, Notice> kvp in _allNotices)
            {
                if (kvp.Value.Status == Status.unavailable && kvp.Value.GetNoticeType() == type)
                    result.Add(kvp.Value);
            }

            return result;
        }
        internal ICollection<Notice> GetAllNotices(JobType type)
        {
            List<Notice> result = new List<Notice>();

            foreach (KeyValuePair<Int64, Notice> kvp in _allNotices)
            {
                if (kvp.Value.GetNoticeType() == type)
                    result.Add(kvp.Value);
            }

            return result;
        }
        #endregion
    }



}

