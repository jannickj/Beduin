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
    public class ConvertProbedVerticesToPercept : JSConverterToForeign<ProbedVerticesMessage, IilPercept>
    {
        public override IilPercept BeginConversionToForeign(ProbedVerticesMessage gobj)
        {
            var vertices = gobj.ProbedVertices.OfType<ProbedVertexMessage>().Select(v => new IilFunction("probedVertex",
                new IilFunction("name", new IilIdentifier(v.Name)), new IilFunction("value", new IilNumeral(v.Value)))).ToArray();
            return new IilPercept("probedVertices", vertices);
        }
    }
}
