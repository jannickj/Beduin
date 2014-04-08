using JSLibrary.Conversion;
using JSLibrary.IiLang;
using NabfProject.Parsers.AgentMasterToAgentConverters;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using XmasEngineModel.Management;
using NabfProject.Parsers.KnowledgeConverters;

namespace NabfProject.Parsers
{
    public class AgentMasterToAgentParser : JSConversionTool<XmasEvent, IilPerceptCollection>
    {

        public AgentMasterToAgentParser()
        {

            this.AddConverter(new ConverterReceivedJob()
            {
                Parsers = new AgentMasterDataParsers()
            });
            this.AddConverter(new ConverterNewKnowledge() { 
                Parsers = new AgentMasterDataParsers()
            });
            this.AddConverter(new ConverterNewNotice()
            {
                Parsers = new AgentMasterDataParsers()
            });
            this.AddConverter(new ConverterNoticeRemoved()
            {
                Parsers = new AgentMasterDataParsers()
            });
            this.AddConverter(new ConverterNoticeUpdated()
            {
                Parsers = new AgentMasterDataParsers()
            });
            this.AddConverter(new ConverterFiredFromJob()
            {
                Parsers = new AgentMasterDataParsers()
            });

            this.AddConverter(new ConverterRoundChanged());

        }

    }
}
