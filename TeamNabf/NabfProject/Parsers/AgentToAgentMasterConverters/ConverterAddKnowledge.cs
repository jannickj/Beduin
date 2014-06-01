using JSLibrary.Conversion;
using JSLibrary.IiLang.DataContainers;
using NabfProject.Actions;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.IiLang.Parameters;
using NabfProject.KnowledgeManagerModel;
using NabfProject.Parsers.KnowledgeConverters;
using JSLibrary.IiLang;

namespace NabfProject.Parsers.AgentToAgentMasterConverters
{
    public class ConverterAddKnowledge : JSConverterToForeign<IilAction, AddKnowledgeAction>
    {
        public AgentMasterDataParsers MasterDataParser { get; set; }

        public override object KnownID
        {
            get
            {
                return "addKnowledgeAction";
            }
        }

        public override AddKnowledgeAction BeginConversionToForeign(IilAction gobj)
        {
            int simId = (int)((IilNumeral)gobj.Parameters[0]).Value;

            List<Knowledge> knowledge = ((IilFunction)gobj.Parameters[1]).Parameters
                .Select(k => (Knowledge)MasterDataParser.ConvertToKnown(k)).ToList();

            AddKnowledgeAction aka = new AddKnowledgeAction(simId, knowledge);

            return aka;
        }
    }
}
