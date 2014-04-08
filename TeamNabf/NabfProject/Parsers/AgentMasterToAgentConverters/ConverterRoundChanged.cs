using JSLibrary.Conversion;
using JSLibrary.IiLang;
using NabfProject.Events;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;
using NabfProject.Parsers.KnowledgeConverters;
using NabfProject.KnowledgeManagerModel;

namespace NabfProject.Parsers.AgentMasterToAgentConverters
{
    public class ConverterRoundChanged : JSConverterToForeign<RoundChangedEvent, IilPerceptCollection>
    {

        public override IilPerceptCollection BeginConversionToForeign(RoundChangedEvent gobj)
        {            

            IilPerceptCollection ipc = new IilPerceptCollection
                (
                new IilPercept("roundChanged", new IilNumeral(gobj.Round))
                );            

            return ipc;
        }
    }
}
