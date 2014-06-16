using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NabfProject.AI;

namespace NabfProject.NoticeBoardModel
{
    public abstract class NoticeBoardHelpers
    {
        public static NoticeBoard.JobType NoticeToJobType(Notice no)
        {
            if (no == null)
                throw new ArgumentNullException("Input to method NoticeToType was null.");

            if (no is DisruptJob)
                return NoticeBoard.JobType.Disrupt;
            else if (no is AttackJob)
                return NoticeBoard.JobType.Attack;
            else if (no is OccupyJob)
                return NoticeBoard.JobType.Occupy;
            else if (no is RepairJob)
                return NoticeBoard.JobType.Repair;
            else if (no is EmptyJob)
                return NoticeBoard.JobType.Empty;
            else
                throw new ArgumentException("Input to NoticeToJobtype, object : " + no.GetType().Name + " was not of appropriate type. It's type was: " + no.GetType());
        }

        public static bool AgentListContainsAgent(ICollection<NabfAgent> agentList, NabfAgent agent)
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
