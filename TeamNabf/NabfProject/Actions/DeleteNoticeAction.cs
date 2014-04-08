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
    public class DeleteNoticeAction : EntityXmasAction<NabfAgent>
    {
        private int SimId;
        private Int64 NoticeId;

        public DeleteNoticeAction(int simID, Int64 noticeId)
        {
            SimId = simID;
            NoticeId = noticeId;
        }

        protected override void Execute()
        {
            SimulationManager simMan = ((NabfModel)this.Engine).SimulationManager;

            simMan.RemoveNotice(SimId, NoticeId);
        }
    }
}
