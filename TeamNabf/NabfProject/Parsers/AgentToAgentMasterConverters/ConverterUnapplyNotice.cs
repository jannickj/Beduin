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
    public class ConverterUnapplyNotice : JSConverterToForeign<IilAction, UnapplyNoticeAction>
    {
        public override object KnownID
        {
            get
            {
                return "unapplyNoticeAction";
            }
        }


        public override UnapplyNoticeAction BeginConversionToForeign(IilAction gobj)
        {
            var bonusfunc = gobj;

            int simId = (int)((IilNumeral)bonusfunc.Parameters[0]).Value;

            int noticeId = (int)((IilNumeral)bonusfunc.Parameters[1]).Value;

            UnapplyNoticeAction una = new UnapplyNoticeAction(simId, noticeId);

            return una;
        }
    }
}
