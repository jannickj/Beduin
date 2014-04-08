using JSLibrary.Conversion;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;
using NabfProject.ServerMessages;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace NabfProject.Parsers.MarsToAgentConverters
{
    public class ConvertSimulationToPercept : JSConverterToForeign<SimulationMessage,IilPercept>
    {
        public override IilPercept BeginConversionToForeign(SimulationMessage gobj)
        {
            var percept = new IilPercept(gobj.MessageName, new IilFunction("step", new IilNumeral(gobj.Step)));
            return percept;
        }
    }
}
