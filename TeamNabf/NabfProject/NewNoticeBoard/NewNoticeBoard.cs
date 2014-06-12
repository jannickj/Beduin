using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NabfProject.AI;
using NabfProject.Events;
using NabfProject.KnowledgeManagerModel;
using JSLibrary;

namespace NabfProject.NewNoticeBoardModel
{
    public class NewNoticeBoard : NoticeBoardHelpers
    {
        private Int64 _freeID = 0;
        private HashSet<NabfAgent> _sharingList = new HashSet<NabfAgent>();
        private Dictionary<Int64, NewNotice> _allNotices = new Dictionary<Int64, NewNotice>();

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
                if (a.Name == a.Name)
                {
                    agentHasApplied = true;
                    break;
                }
            }

            if (agentHasApplied == false)
                return false;

            bool agentHadJob = AgentListContainsAgent(noticeUnappliedTo.GetAgentsOnJob(), agent);

            noticeUnappliedTo.Unapply(agent);

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
                noticeUnappliedTo.AvgDesirabilityAmongTopDesires = -1;
            }

            return true;
        }

        //agents must still be in AgentsApplied list when they get the job
        #region AssignJobs
        public bool AssignJobs()
        {
            Queue<NewNotice> jobQueue;
            CreateQueueSortedByAvgDesirability(out jobQueue);

            while(true/*queue is not empty*/)
            {
                CreateQueueSortedByAvgDesirability(out jobQueue);
            }





            return true;
        }

        private void CreateQueueSortedByAvgDesirability(out Queue<NewNotice> jobQueue)
        {
            jobQueue = new Queue<NewNotice>();
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

