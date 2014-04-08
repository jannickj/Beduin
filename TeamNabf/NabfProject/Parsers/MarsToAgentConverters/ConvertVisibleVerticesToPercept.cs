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
    public class ConvertVisibleVerticesToPercept : JSConverterToForeign<VisibleVerticesMessage, IilPercept>
    {
        public override IilPercept BeginConversionToForeign(VisibleVerticesMessage gobj)
        {
            var vertices = gobj.VisibleVertices.OfType<VisibleVertexMessage>().Select(v => new IilFunction("visibleVertex", 
                new IilFunction("name", new IilIdentifier(v.Name)), new IilFunction("team", new IilIdentifier(v.Team)))).ToArray();
            return new IilPercept("visibleVertices", vertices);
        }
    }
}
