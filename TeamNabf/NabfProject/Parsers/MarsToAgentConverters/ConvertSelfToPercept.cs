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
    public class ConvertSelfToPercept : JSConverterToForeign<SelfMessage, IilPercept>
    {    
        public override IilPercept BeginConversionToForeign(SelfMessage gobj)
        {
            return new IilPercept("self", new IilFunction("energy", new IilNumeral(gobj.Energy))
                , new IilFunction("health", new IilNumeral(gobj.Health)), new IilFunction("lastAction", new IilIdentifier(gobj.LastAction))
                , new IilFunction("lastActionParam",new IilIdentifier(gobj.LastActionParam)), new IilFunction("lastActionResult",new IilIdentifier(gobj.LastActionResult))
                , new IilFunction("maxEnergy", new IilNumeral(gobj.MaxEnergy)), new IilFunction("maxEnergyDisabled", new IilNumeral(gobj.MaxEnergyDisabled))
                , new IilFunction("maxHealth", new IilNumeral(gobj.MaxHealth)), new IilFunction("position", new IilIdentifier(gobj.Position))
                , new IilFunction("strength",new IilNumeral(gobj.Strength)), new IilFunction("visRange",new IilNumeral(gobj.VisRange))
                , new IilFunction("zoneScore",new IilNumeral(gobj.ZoneScore)));
        }
    }
}
