using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NabfProject.AI;
using NabfProject.SimManager;
using XmasEngineModel.Management;
using XmasEngineModel.Management.Actions;

namespace NabfProject.Actions
{
    public class NewRoundAction : EntityXmasAction<NabfAgent>
    {
        public int RoundNumber { get; private set; }
        public int SimId { get; private set; }

        public NewRoundAction(int simID, int roundNumber)
        {
            RoundNumber = roundNumber;
            SimId = simID;
        }

        protected override void Execute()
        {
            SimulationManager simMan = ((NabfModel)this.Engine).SimulationManager;

            bool newRound = simMan.TryGoNextRound(SimId, RoundNumber);

            //if (newRound)
            //{
            //    TimedAction ta = this.Factory.CreateTimer(() =>
            //        {
            //            simMan.FindJobs(SimId);
            //        }
            //        );
            //    ta.SetSingle(simMan.TimeBeforeApplyCloses);
            //    this.RunAction(ta);
            //}
        }
    }
}
