using NabfProject.AI;
using NabfProject.SimManager;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using XmasEngineModel.EntityLib;
using XmasEngineModel.Management;
using NabfProject.NoticeBoardModel;

namespace NabfProject.Actions
{
    public class ApplyNoticeAction : EntityXmasAction<NabfAgent>
    {
        private int SimId;
        private Int64 noticeId;
        private int Desired;

        public Int64 NoticeId
        {
            get { return noticeId; }
        }

        public ApplyNoticeAction(int simID, Int64 noticeId, int desired)
        {
            SimId = simID;
            this.noticeId = noticeId;
            Desired = desired;
        }

        protected override void Execute()
        {
            SimulationManager simMan = ((NabfModel)this.Engine).SimulationManager;
			simMan.ApplyToNotice(SimId, NoticeId, Desired, this.Source);
        }

		public override string ToString()
		{
			SimulationManager simMan = ((NabfModel)this.Engine).SimulationManager;
			return "Apply to " + NoticeId + " at round: " + simMan.CurrentRoundNumber + " desire: " + this.Desired;
		}
    }
}
