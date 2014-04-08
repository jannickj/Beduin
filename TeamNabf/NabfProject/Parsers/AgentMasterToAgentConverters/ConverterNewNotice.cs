using JSLibrary.Conversion;
using JSLibrary.IiLang;
using NabfProject.Events;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.IiLang.DataContainers;

namespace NabfProject.Parsers.AgentMasterToAgentConverters
{
    public class ConverterNewNotice : JSConverterToForeign<NewNoticeEvent,IilPerceptCollection>
    {
        public AgentMasterDataParsers Parsers { get; set; }

        public override IilPerceptCollection BeginConversionToForeign(NewNoticeEvent gobj)
        {
            IilPerceptCollection ipc;
            IilPercept percept = ((IilPerceptCollection)Parsers.ConvertToForeign(gobj.Notice)).Percepts[0];

            ipc = new IilPerceptCollection
            (
                new IilPercept("newNotice"),
                percept
            );

            return ipc;
        }
    }
}
