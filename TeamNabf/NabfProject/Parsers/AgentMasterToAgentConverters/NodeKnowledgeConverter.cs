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
            //  <IilIdentifier name="?" />
            //  <IilNumeral value=? />
            //</ IilAction>
            var ia = (IilFunction)fobj;

            var identifier = (IilIdentifier)ia.Parameters[0];
            var numeral = (IilNumeral)ia.Parameters[1];

            int value = (int)numeral.Value;

            NodeKnowledge nk = new NodeKnowledge(identifier.Value, value);

            return nk;
        }

        public override IilElement BeginConversionToForeign(NodeKnowledge gobj)
        {
            if (gobj.Value == 0)
            {
                return new IilPerceptCollection(
					new IilPercept("visibleVertices"
							,new IilFunction("visibleVertex"
									, new IilFunction("name"
										, new IilIdentifier(gobj.Name))))
                    );
            }
            else
            {
                return new IilPerceptCollection(
					new IilPercept("probedVertices"
						,new IilFunction("probedVertex"
							, new IilFunction("name", new IilIdentifier(gobj.Name))
							, new IilFunction("value", new IilNumeral(gobj.Value)))
                    ));
            }
        }
    }
}
