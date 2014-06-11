using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NabfProject.AI;
using NabfProject.Events;
using NabfProject.KnowledgeManagerModel;
using JSLibrary;

namespace NabfProject.NewNoticeBoard
{
    public class NewNoticeBoard : NoticeLib
    {
        private Int64 _freeID = 0;
        private HashSet<NabfAgent> _sharingList = new HashSet<NabfAgent>();
        private HashSet<NewNotice> _allNotices = new HashSet<NewNotice>();

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



        #region legacy backup
        //private bool RaiseEventForNotice(NewNotice n, bool fireOtherAtEnd)
        //{
        //    //a.Raise(new ReceivedJobEvent(n, a));
        //    return true;
        //}
        //private bool RaiseFiredEventForNotice(NewNotice n, NabfAgent a)
        //{
        //    //a.Raise(new FiredFromJobEvent(n, a));

        //    return true;
        //}

        //public void SendOutAllNoticesToAgent(NabfAgent agent)
        //{
        //    //foreach (KeyValuePair<JobType, Notice[]> kvp in _availableJobs)
        //    //{
        //    //    foreach (Notice n in kvp.Value)
        //    //    {
        //    //        if (n.Status == Status.available)
        //    //            agent.Raise(new NewNoticeEvent(n));
        //    //    }
        //    //}
        //}
        #endregion
    }
}
