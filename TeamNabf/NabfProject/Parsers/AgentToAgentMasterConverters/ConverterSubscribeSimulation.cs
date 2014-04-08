using JSLibrary.Conversion;
using JSLibrary.IiLang.DataContainers;
using NabfProject.Actions;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.IiLang.Parameters;

namespace NabfProject.Parsers.AgentToAgentMasterConverters
{
    public class ConverterSubscribeSimulation : JSConverterToForeign<IilAction, SubscribeSimulationAction>
    {
        public override object KnownID
        {
            get
            {
                return "subscribeSimulationAction";
            }
        }

        public override SubscribeSimulationAction BeginConversionToForeign(IilAction gobj)
        {
            int simId = (int)((IilNumeral)gobj.Parameters[0]).Value;

            SubscribeSimulationAction ssa = new SubscribeSimulationAction(simId);

            return ssa;
        }
    }
}
