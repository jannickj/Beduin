using JSLibrary.Conversion;
using JSLibrary.IiLang;
using NabfProject.Events;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;

namespace NabfProject.Parsers.AgentMasterToAgentConverters
{
    public class ConverterNoticeUpdated : JSConverterToForeign<NoticeUpdatedEvent, IilPerceptCollection>
    {
        public AgentMasterDataParsers Parsers { get; set; }

        public override IilPerceptCollection BeginConversionToForeign(NoticeUpdatedEvent gobj)
        {
            IilPerceptCollection ipc;
            IilPercept percept = ((IilPerceptCollection)Parsers.ConvertToForeign(gobj.UpdatedNotice)).Percepts[0];

            ipc = new IilPerceptCollection
            (
                new IilPercept("noticeUpdated"),
                percept
            );

            return ipc;
        }
    }
}
