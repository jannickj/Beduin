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
    public class ConvertVisibleEdgesToPercept : JSConverterToForeign<VisibleEdgesMessage, IilPercept>
    {
        public override IilPercept BeginConversionToForeign(VisibleEdgesMessage gobj)
        {
            var edges = gobj.VisibleEdges.OfType<VisibleEdgeMessage>().Select(v => new IilFunction("visibleEdge",
                new IilFunction("node1", new IilIdentifier(v.Node1)), new IilFunction("node2", new IilIdentifier(v.Node2)))).ToArray();
            return new IilPercept("visibleEdges", edges);
        }
    }
}
