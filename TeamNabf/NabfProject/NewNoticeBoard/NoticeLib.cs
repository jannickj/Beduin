using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NabfProject.AI;

namespace NabfProject.NewNoticeBoardModel
{
    public abstract class NoticeBoardHelpers
    {
        public NewNoticeBoard.JobType NoticeToJobType(NewNotice no)
        {
            if (no == null)
                throw new ArgumentNullException("Input to method NoticeToType was null.");

            if (no is DisruptJob)
                return NewNoticeBoard.JobType.Disrupt;
            else if (no is AttackJob)
                return NewNoticeBoard.JobType.Attack;
            else if (no is OccupyJob)
                return NewNoticeBoard.JobType.Occupy;
            else if (no is RepairJob)
                return NewNoticeBoard.JobType.Repair;
            else if (no is EmptyJob)
                return NewNoticeBoard.JobType.Empty;
            else
                throw new ArgumentException("Input to NoticeToJobtype, object : " + no.GetType().Name + " was not of appropriate type. It's type was: " + no.GetType());
        }

        public bool AgentListContainsAgent(ICollection<NabfAgent> agentList, NabfAgent agent)
        {
            foreach (NabfAgent a in agentList)
            {
                if (a.Name == agent.Name)
                    return true;
            }
            return false;
        }
    }
}
