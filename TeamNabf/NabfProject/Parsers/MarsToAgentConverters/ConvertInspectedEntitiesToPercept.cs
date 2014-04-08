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
    public class ConvertInspectedEntitiesToPercept : JSConverterToForeign<InspectedEntitiesMessage, IilPercept>
    {
        public override IilPercept BeginConversionToForeign(InspectedEntitiesMessage gobj)
        {
            var entities = gobj.InspectedEntities.OfType<InspectedEntityMessage>().Select(v => new IilFunction("inspectedEntity",
                new IilFunction("energy", new IilNumeral(v.Energy)), new IilFunction("health", new IilNumeral(v.Health)),
                new IilFunction("maxEnergy", new IilNumeral(v.MaxEnergy)), new IilFunction("maxHealth", new IilNumeral(v.MaxHealth)),
                new IilFunction("name", new IilIdentifier(v.Name)), new IilFunction("node", new IilIdentifier(v.Node)),
                new IilFunction("role", new IilIdentifier(v.Role)), new IilFunction("strength", new IilNumeral(v.Strength)),
                new IilFunction("team", new IilIdentifier(v.Team)), new IilFunction("visRange", new IilNumeral(v.VisRange))
                )).ToArray();
            return new IilPercept("inspectedEntities", entities);
        }
    }
}
