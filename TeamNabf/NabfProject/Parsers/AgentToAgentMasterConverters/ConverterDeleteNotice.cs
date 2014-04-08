using JSLibrary.Conversion;
using JSLibrary.IiLang.DataContainers;
using NabfProject.Actions;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.IiLang.Parameters;

namespace NabfProject.Parsers.AgentToAgentMasterConverters
{
    public class ConverterDeleteNotice : JSConverterToForeign<IilAction, DeleteNoticeAction>
    {
        public override object KnownID
        {
            get
            {
                return "deleteNoticeAction";
            }
        }

        public override DeleteNoticeAction BeginConversionToForeign(IilAction gobj)
        {
            var bonusfunc = gobj;

            int simId = (int)((IilNumeral)bonusfunc.Parameters[0]).Value;

            Int64 noticeId = (Int64)((IilNumeral)bonusfunc.Parameters[1]).Value;

            DeleteNoticeAction dna = new DeleteNoticeAction(simId, noticeId);

            return dna;
        }
    }
}
