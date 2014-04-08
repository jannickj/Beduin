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
    public class NodeKnowledgeConverter : JSConverter<NodeKnowledge, IilElement>
    {
        public override object ForeignID
        {
            get
            {
                return "nodeKnowledge";
            }
        }

        public override NodeKnowledge BeginConversionToKnown(IilElement fobj)
        {
            //<IilAction name="nodeKnowledge" >
            //  <IilNumeral value=? />
            //  <IilIdentifier name="?" />
            //</ IilAction>
            var ia = (IilAction)fobj;

            var numeral = (IilNumeral)ia.Parameters[0];
            var identifier = (IilIdentifier)ia.Parameters[1];

            NodeKnowledge nk = new NodeKnowledge(identifier.Value, (int)numeral.Value);

            return nk;
        }

        public override IilElement BeginConversionToForeign(NodeKnowledge gobj)
        {
            return new IilPerceptCollection(new IilPercept("nodeKnowledge"
                , new IilFunction("nodeVal", new IilNumeral(gobj.Value))
                , new IilFunction("nodeName", new IilIdentifier(gobj.Name))
                ));
        }
    }
}
