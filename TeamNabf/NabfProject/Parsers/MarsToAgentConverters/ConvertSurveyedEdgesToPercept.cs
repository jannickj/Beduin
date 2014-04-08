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
    public class ConvertSurveyedEdgesToPercept : JSConverterToForeign<SurveyedEdgesMessage, IilPercept>
    {
        public override IilPercept BeginConversionToForeign(SurveyedEdgesMessage gobj)
        {
            var edges = gobj.SurveyedEdges.OfType<SurveyedEdgeMessage>().Select(v => new IilFunction("surveyedEdge",
                new IilFunction("node1", new IilIdentifier(v.Node1)), new IilFunction("node2", new IilIdentifier(v.Node2)),
                new IilFunction("weight", new IilNumeral(v.Weight)))).ToArray();
            return new IilPercept("surveyedEdges", edges);
        }
    }
}
