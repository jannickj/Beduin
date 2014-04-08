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
    public class ConvertVisibleEntitiesToPercept : JSConverterToForeign<VisibleEntitiesMessage, IilPercept>
    {
        public override IilPercept BeginConversionToForeign(VisibleEntitiesMessage gobj)
        {
            var entities = gobj.VisibleEntities.OfType<VisibleEntityMessage>().Select(v => new IilFunction("visibleEntity",
                new IilFunction("name", new IilIdentifier(v.Name)), new IilFunction("team", new IilIdentifier(v.Team)),
                new IilFunction("node", new IilIdentifier(v.Node)), new IilFunction("status", new IilIdentifier(v.Status)))).ToArray();
            return new IilPercept("visibleEntities", entities);
        }
    }
}
