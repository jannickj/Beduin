using NabfProject.SimManager;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using XmasEngine;
using XmasEngineModel.Management;

namespace NabfProject
{
    public class NabfModelFactory : XmasModelFactory
    {
        public override XmasEngineModel.XmasModel ConstructModel(XmasEngineModel.XmasWorldBuilder builder)
        {
            SimulationFactory simfactor = new SimulationFactory();
            SimulationManager simman = new SimulationManager(simfactor);
            EventManager evtman = ConstructEventManager();
            ActionManager actman = ConstructActionManager(evtman);
            XmasFactory fact = ConstructFactory(actman);
            NabfModel model = new NabfModel(simman, builder, actman, evtman, fact);
            return model;
        }
        
    }
}
