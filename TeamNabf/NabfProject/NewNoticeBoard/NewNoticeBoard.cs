using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NabfProject.AI;
using NabfProject.Events;
using NabfProject.KnowledgeManagerModel;
using JSLibrary;
using JSLibrary.Data;

namespace NabfProject.NewNoticeBoardModel
{
    public class NewNoticeBoard : NoticeBoardHelpers
    {
        private Int64 _freeID = 0;
        private HashSet<NabfAgent> _sharingList = new HashSet<NabfAgent>();
        private Dictionary<Int64, NewNotice> _allNotices = new Dictionary<Int64, NewNotice>();
        private DictionaryList<string, Int64> _agentToAppliedNotices = new DictionaryList<string, Int64>();

        public enum JobType { Empty = 0, Occupy = 1, Repair = 2, Disrupt = 3, Attack = 4 }
        public enum Status { available, unavailable }

        private const bool verbose = false;
        //status reporting for SimMan
        public int _agentsFiredCounter = 0;
        public int _nonUniqueJobsAttemptedToBeAdded = 0;
        public int _createdOccupyJob = 0;
        public int _createdAttackJob = 0;
        public int _createdRepairJob = 0;
        public int _createdDisruptJob = 0;

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
        public ICollection<NabfAgent> GetSubscribedAgents()
        {
            return _sharingList.ToList();
        }

        public void AddNoticeToAllNotices(NewNotice n)
        {
            _allNotices.Add(n.Id, n);
        }
        public ICollection<NewNotice> GetAllNotices()
        {
            return _allNotices.Values.ToList();
        }
        public NewNotice GetNoticeById(Int64 id)
        {
            return _allNotices[id];
        }

        

        /// <summary>
        /// Sends out all notices to an agent. 
        /// Only to be used when reconnecting after a disconnect.
        /// </summary>
        /// <param name="agent">Agent to send data to</param>
        public void SendOutAllNoticesToAgent(NabfAgent agent)
        {
            foreach (KeyValuePair<Int64, NewNotice> kvp in _allNotices)
            {
                //agent.Raise(new NewNoticeEvent(n));
            }
        }
        

        public bool CreateNotice(JobType jobType, int agentsNeeded, List<NodeKnowledge> whichNodesIsInvolvedInJob, List<NodeKnowledge> whichNodesToStandOn, string agentToRepair, int jobValue)
        {
            NewNotice n = null;
            Int64 id = _freeID;
            _freeID++;
            switch (jobType)
            {
                case JobType.Attack:
                    n = new AttackJob(agentsNeeded, whichNodesIsInvolvedInJob, jobValue, id);
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
        private bool AddNotice(NewNotice n)
        {
            bool isUnique = true;
            foreach (NewNotice job in GetAllNotices())
            {
                if (job.ContentIsEqualTo(n) || job.ContentIsSubsetOf(n) || job.Equals(n))
                    isUnique = false;
            }

            if (!isUnique)
            {
                _nonUniqueJobsAttemptedToBeAdded++;
                if (verbose && _nonUniqueJobsAttemptedToBeAdded % 2 == 0)
                    Console.WriteLine("Total number of received non-unique jobs: " + _nonUniqueJobsAttemptedToBeAdded);
                return false;
            }

            AddNoticeToAllNotices(n);

            NoticeBoardModel.OccupyJob testNotice_TO_BE_REMOVED 
                = new NoticeBoardModel.OccupyJob(0, new List<NodeKnowledge>(), new List<NodeKnowledge>(), 0, 0);//remove this once event has been changed to new notice class

            foreach (NabfAgent a in _sharingList)
            {
                a.Raise(new NewNoticeEvent(testNotice_TO_BE_REMOVED));//fix this once event has been changed to new notice class
            }
            if (n is OccupyJob)
                _createdOccupyJob++;
            if (n is RepairJob)
                _createdRepairJob++;
            if (n is DisruptJob)
                _createdDisruptJob++;
            if (n is AttackJob)
                _createdAttackJob++;

            return true;
        }
        public bool UpdateNotice(int id, List<NodeKnowledge> whichNodesIsInvolvedInJob, List<NodeKnowledge> whichNodesToStandOn, int agentsNeeded, int jobValue, string agentToRepair)
        {
            NewNotice no;
            bool b = _allNotices.TryGetValue(id, out no);

            if (b == false)
                return false;

            no.UpdateNotice(whichNodesIsInvolvedInJob, agentsNeeded, jobValue, whichNodesToStandOn, agentToRepair);

            foreach (NabfAgent a in _sharingList)
            {
                //a.Raise(new NoticeUpdatedEvent(id, no));
            }

            return true;
        }
        public bool DeleteNotice(int id)
        {
            NewNotice notice;
            bool b = _allNotices.TryGetValue(id, out notice);
            if (b == false)
                return false;

            foreach (NabfAgent a in notice.GetAgentsOnJob())
            {
                //FireAgentFromNotice(a,notice);
            }
            _allNotices.Remove(notice.Id);

            foreach (NabfAgent a in _sharingList)
            {
                //a.Raise(new NoticeRemovedEvent(no));
            }

            return true;
        }
                

        public bool ApplyToNotice(NabfAgent agent, Int64 idToApplyTo, int desireAppliedWith)
        {
            NewNotice noticeAppliedTo = GetNoticeFromId(idToApplyTo);
            if (noticeAppliedTo == null)
                return false;

            noticeAppliedTo.Apply(desireAppliedWith, agent);
            _agentToAppliedNotices.Add(agent.Name, noticeAppliedTo.Id);

            return true;
        }
        public bool UnapplyToNotice(NabfAgent agent, Int64 idToUnapplyTo)
        {
            NewNotice noticeUnappliedTo = GetNoticeFromId(idToUnapplyTo);
            if (noticeUnappliedTo == null)
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

            noticeUnappliedTo.Unapply(agent);
            _agentToAppliedNotices.Remove(agent.Name, noticeUnappliedTo.Id);

            //if agent has the job, fire the rest recursively and resset the notice.
            if (agentHadJob)
            {
                foreach (NabfAgent a in noticeUnappliedTo.GetAgentsOnJob())
                {
                    UnapplyToNotice(a, noticeUnappliedTo.Id);
                    RaiseFiredEventForNotice(noticeUnappliedTo, a);
                    _agentsFiredCounter++;
                    if (verbose && _agentsFiredCounter % 10 == 0)
                        Console.WriteLine("Total number of fired agents: " + _agentsFiredCounter);
                }
                noticeUnappliedTo.Status = Status.available;
                //noticeUnappliedTo.AvgDesirabilityAmongTopDesires = -1;
            }

            return true;
        }

        //agents must still be in AgentsApplied list when they get the job
        #region AssignJobs
        public bool AssignJobs()
        {
            NewNotice notice, nextNotice;
            bool agentsAlsoAppearAsTopDesiresOnNextNotice = false;
            List<Int64> noticesToUnapplyFrom;            

            Queue<NewNotice> jobQueue = CreateQueueSortedByAvgDesirability();

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
                    agentsAlsoAppearAsTopDesiresOnNextNotice = false;
                    //a.Raise(new ReceivedJobEvent(notice, agent));

                    #region checks if queue needs re-ordering
                    if (jobQueue.Count > 0)
                    {
                        nextNotice = jobQueue.Peek();
                        if (nextNotice.GetAgentProspects().Contains<NabfAgent>(agent))
                            agentsAlsoAppearAsTopDesiresOnNextNotice = true;
                    }
                    #endregion

                    noticesToUnapplyFrom = _agentToAppliedNotices[agent.Name].ToList();
                    noticesToUnapplyFrom.Remove(notice.Id);
                    foreach (Int64 noticeId in noticesToUnapplyFrom)
                    {
                        UnapplyToNotice(agent, noticeId);
                    }
                }

                #region re-orders the queue if needed
                if (agentsAlsoAppearAsTopDesiresOnNextNotice)
                {
                    jobQueue = CreateQueueSortedByAvgDesirability();
                }
                #endregion
            }
            return true;
        }

        //Creates a queue of jobs, sorted after average desirability, which has AgentsApplied >= AgentsNeeded. 
        //Average desirability is calculated here as:
        //the sum of the desire of the K agents with highest desire, divided by K
        //The method also puts the top K agents into AgentProspects
        public Queue<NewNotice> CreateQueueSortedByAvgDesirability()
        {
            Queue<NewNotice> jobQueue = new Queue<NewNotice>();
            List<NabfAgent> namesOfTopContenders;
            //SortedList<double, NewNotice> sortedNoticeList = new SortedList<double, NewNotice>(new InvertedComparer<double>());//the list now sorts descending
            List<KeyValuePair<double, NewNotice>> noticeList = new List<KeyValuePair<double, NewNotice>>();
            foreach (NewNotice n in GetAllNotices())
            {
                n.ClearAgentProspects();
            }
            

            foreach (NewNotice notice in GetAllNotices())
            {
                if (notice.GetAgentsApplied().Count >= notice.AgentsNeeded && notice.Status == Status.available)
                {
                    noticeList.Add(new KeyValuePair<double, NewNotice>(CalculateAverageDesireForTopContenders(notice, out namesOfTopContenders), notice));
                    notice.AddRangeToAgentProspects(namesOfTopContenders);
                }
            }

            noticeList.Sort(new KvpKeyInvertedComparer<double, NewNotice>());

            for (int i = 0; i < noticeList.Count; i++)
            {
                jobQueue.Enqueue((NewNotice)noticeList[i].Value);
            }

            return jobQueue;
        }
        public double CalculateAverageDesireForTopContenders(NewNotice notice, out List<NabfAgent> namesOfTopContenders)
        {
            double result = -1.0;
            int desire;
            bool desireFound;
            namesOfTopContenders = new List<NabfAgent>();
            //SortedList<int, NabfAgent> sortedAgentList = new SortedList<int, NabfAgent>(new InvertedComparer<int>());//the list now sorts descending
            List<KeyValuePair<int, NabfAgent>> agentList = new List<KeyValuePair<int, NabfAgent>>();

            foreach (NabfAgent agent in notice.GetAgentsApplied())
            {
                desireFound = notice.TryGetDesirabilityOfAgent(agent, out desire);
                if (desireFound)
                {
                    agentList.Add(new KeyValuePair<int, NabfAgent>(desire, agent));
                    //sortedAgentList.Add(desire, agent);
                }
            }
            agentList.Sort(new KvpKeyInvertedComparer<int, NabfAgent>());

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
        #endregion

        #region private helper functions
        private NewNotice GetNoticeFromId(Int64 inputId)
        {
            NewNotice result = null;
            foreach (NewNotice n in GetAllNotices())
            {
                if (n.Id == inputId)
                {
                    result = n;
                    break;
                }
            }
            return result;
        }
        #endregion

        #region legacy backup
        private bool RaiseEventForNotice(NewNotice n, bool fireOtherAtEnd)
        {
            //a.Raise(new ReceivedJobEvent(n, a));
            return true;
        }
        private bool RaiseFiredEventForNotice(NewNotice n, NabfAgent a)
        {
            //a.Raise(new FiredFromJobEvent(n, a));

            return true;
        }
        #endregion

    }



}

