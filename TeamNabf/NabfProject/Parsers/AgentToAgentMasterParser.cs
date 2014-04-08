using JSLibrary.Conversion;
using JSLibrary.IiLang.DataContainers;
using NabfProject.AI;
using NabfProject.Parsers.AgentToAgentMasterConverters;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using XmasEngineModel.Management;

namespace NabfProject.Parsers
{
    public class AgentToAgentMasterParser : JSConversionTool<IilAction, EntityXmasAction<NabfAgent>>
    {
        public AgentToAgentMasterParser()
        {
            this.IdOfKnown = new JSConversionIDFetcherSimple<IilAction>(GetIdFromIilAction);

            AgentMasterDataParsers amdp = new AgentMasterDataParsers();

            this.AddConverter(new ConverterAddKnowledge() { MasterDataParser = amdp });
            this.AddConverter(new ConverterApplyNotice());
            this.AddConverter(new ConverterChangeNotice() { MasterDataParser = amdp });
            this.AddConverter(new ConverterCreateNotice() { MasterDataParser = amdp });
            this.AddConverter(new ConverterDeleteNotice());
            this.AddConverter(new ConverterUnapplyNotice());
            this.AddConverter(new ConverterSubscribeSimulation());
            this.AddConverter(new ConverterNewRound());
        }

        private object GetIdFromIilAction(IilAction a)
        {
            return a.Name;
        }
    }
}
