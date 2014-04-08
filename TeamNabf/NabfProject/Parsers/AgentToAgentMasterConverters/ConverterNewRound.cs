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
    public class ConverterNewRound : JSConverterToForeign<IilAction, NewRoundAction>
    {
        public override object KnownID
        {
            get
            {
                return "newRoundAction";
            }
        }

        public override NewRoundAction BeginConversionToForeign(IilAction gobj)
        {
            int simId = (int)((IilNumeral)gobj.Parameters[0]).Value;

            int roundNumber = (int)((IilNumeral)gobj.Parameters[1]).Value;

            NewRoundAction nra = new NewRoundAction(simId, roundNumber);

            return nra;
        }
    }
}
