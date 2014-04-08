using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.Conversion;
using JSLibrary.IiLang;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;
using NabfProject.ServerMessages;

namespace NabfProject.Parsers.MarsToAgentConverters
{
    public class ConvertPerceptionMessageToIilPerceptCollection : JSConverterToForeign<PerceptionMessage, IilPerceptCollection>
    {
        public override IilPerceptCollection BeginConversionToForeign(PerceptionMessage gobj)
        {
            return new IilPerceptCollection(gobj.Elements.Select(ip => ((IilPercept)this.ConvertToForeign(ip))).ToArray());;
        }
    }
}
