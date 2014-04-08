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
    public class ConverterNewKnowledge : JSConverterToForeign<NewKnowledgeEvent, IilPerceptCollection>
    {
        public AgentMasterDataParsers Parsers { get; set; }

        public override IilPerceptCollection BeginConversionToForeign(NewKnowledgeEvent gobj)
        {            
            IilPercept knowledgePercept = ((IilPerceptCollection)Parsers.ConvertToForeign(gobj.NewKnowledge)).Percepts[0];

            IilPerceptCollection ipc = new IilPerceptCollection
                (
                new IilPercept("newKnowledge", new IilIdentifier(gobj.NewKnowledge.GetTypeToString())),
                knowledgePercept
                );            

            return ipc;
        }
    }
}
