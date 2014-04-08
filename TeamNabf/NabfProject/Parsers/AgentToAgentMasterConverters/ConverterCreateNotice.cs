using JSLibrary.Conversion;
using JSLibrary.IiLang.DataContainers;
using NabfProject.Actions;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.IiLang.Parameters;
using NabfProject.KnowledgeManagerModel;
using NabfProject.NoticeBoardModel;

namespace NabfProject.Parsers.AgentToAgentMasterConverters
{
    public class ConverterCreateNotice : JSConverterToForeign<IilAction, CreateNoticeAction>
    {
        public AgentMasterDataParsers MasterDataParser { get; set; }

        public override object KnownID
        {
            get
            {
                return "createNoticeAction";
            }
        }


        public override CreateNoticeAction BeginConversionToForeign(IilAction gobj)
        {
            var bonusfunc = gobj;

            int simId = (int)((IilNumeral)bonusfunc.Parameters[0]).Value;

            int type = (int)((IilNumeral)bonusfunc.Parameters[1]).Value;
            NoticeBoard.JobType jobType = (NoticeBoard.JobType)type;

            int agentsNeeded = (int)((IilNumeral)bonusfunc.Parameters[2]).Value;
            
            List<NodeKnowledge> nodes = ((IilFunction)bonusfunc.Parameters[3]).Parameters
                .Select(k => (NodeKnowledge)MasterDataParser.ConvertToKnown(k)).ToList();

            int value = (int)((IilNumeral)bonusfunc.Parameters[4]).Value;

            List<NodeKnowledge> zone = new List<NodeKnowledge>();
            string agentToRepair = "";
            switch (jobType)
            {
                case NoticeBoard.JobType.Occupy:
                    zone = ((IilFunction)bonusfunc.Parameters[5]).Parameters
                        .Select(k => (NodeKnowledge)MasterDataParser.ConvertToKnown(k)).ToList();
                    break;
                case NoticeBoard.JobType.Repair:
                    agentToRepair = ((IilIdentifier)bonusfunc.Parameters[5]).Value;
                    break;
            }

            CreateNoticeAction cna = new CreateNoticeAction(simId, (NoticeBoardModel.NoticeBoard.JobType)jobType, agentsNeeded, nodes, zone, agentToRepair, value);

            return cna;
        }
    }
}
