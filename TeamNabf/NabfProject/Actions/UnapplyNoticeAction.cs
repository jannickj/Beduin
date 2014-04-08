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
    public class UnapplyNoticeAction : EntityXmasAction<NabfAgent>
    {
        private int SimId;
        private Int64 Noticeid;

        public UnapplyNoticeAction(int simID, Int64 id)
        {
            SimId = simID;
            Noticeid = id;
        }
    
        protected override void Execute()
        {
            SimulationManager simMan = ((NabfModel)this.Engine).SimulationManager;

            simMan.UnApplyToNotice(SimId, Noticeid, this.Source);
        }
    }
}
