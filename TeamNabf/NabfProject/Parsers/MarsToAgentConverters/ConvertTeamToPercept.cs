using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.Conversion;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;
using NabfProject.ServerMessages;

namespace NabfProject.Parsers.MarsToAgentConverters
{
    public class ConvertTeamToPercept : JSConverterToForeign<TeamMessage, IilPercept>
    {
        public override IilPercept BeginConversionToForeign(TeamMessage gobj)
        {
            IilIdentifier[] achies;
            if (gobj.Achievements != null)
                achies = gobj.Achievements.AchievementList.OfType<AchievementMessage>().Select(a => new IilIdentifier(a.Name)).ToArray();
            else
                achies = new IilIdentifier[0];
            return new IilPercept("team", new IilFunction("lastStepScore", new IilNumeral(gobj.LastStepScore))
                , new IilFunction("money", new IilNumeral(gobj.Money))
                , new IilFunction("score", new IilNumeral(gobj.Score))
                , new IilFunction("zoneScore", new IilNumeral(gobj.ZonesScore))
                , new IilFunction("achievements", achies)); 
        }
    }
}
