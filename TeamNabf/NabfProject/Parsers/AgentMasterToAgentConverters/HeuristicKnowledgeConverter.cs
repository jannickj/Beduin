using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.IiLang;
using JSLibrary.Conversion;
using NabfProject.KnowledgeManagerModel;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;

namespace NabfProject.Parsers.KnowledgeConverters
{
    public class HeuristicKnowledgeConverter : JSConverter<HeuristicKnowledge, IilElement>
    {
        public override object ForeignID
        {
            get
            {
                return "heuristicKnowledge";
            }
        }

        public override HeuristicKnowledge BeginConversionToKnown(IilElement fobj)
        {
            var ia = (IilFunction)fobj;

            var identifier1 = (IilIdentifier)ia.Parameters[0];
            var identifier2 = (IilIdentifier)ia.Parameters[1];
            var numeral = (IilNumeral)ia.Parameters[2];

            HeuristicKnowledge hk = new HeuristicKnowledge(identifier1.Value, identifier2.Value, (int)numeral.Value);

            return hk;
        }

        public override IilElement BeginConversionToForeign(HeuristicKnowledge gobj)
        {
            return new IilPerceptCollection(new IilPercept("heuristicUpdate"
                    , new IilFunction("heuristicUpdate"
                        , new IilFunction("node1", new IilIdentifier(gobj.Node1))
                        , new IilFunction("node2", new IilIdentifier(gobj.Node2))
                        , new IilFunction("distance", new IilNumeral(gobj.Distance)))
                    ));
        }
    }
}
