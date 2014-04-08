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
    public class EdgeKnowledgeConverter : JSConverter<EdgeKnowledge, IilElement>
    {
        public override object ForeignID
        {
            get
            {
                return "edgeKnowledge";
            }
        }

        public override EdgeKnowledge BeginConversionToKnown(IilElement fobj)
        {
            //<IilAction name="edgeKnowledge" >
            //  <IilIdentifier node1="?" />
            //  <IilIdentifier node2="?" />
            //  <IilNumeral value=? />
            //</ IilAction>
            var ia = (IilAction)fobj;

            var identifier1 = (IilIdentifier)ia.Parameters[0];
            var identifier2 = (IilIdentifier)ia.Parameters[1];
            var numeral = (IilNumeral)ia.Parameters[2];

            EdgeKnowledge ek = new EdgeKnowledge(identifier1.Value, identifier2.Value, (int)numeral.Value);

            return ek;
        }

        public override IilElement BeginConversionToForeign(EdgeKnowledge gobj)
        {
            return new IilPerceptCollection(new IilPercept("edgeKnowledge"
                , new IilFunction("node1", new IilIdentifier(gobj.Node1))
                , new IilFunction("node2", new IilIdentifier(gobj.Node2))
                , new IilFunction("weight", new IilNumeral(gobj.Weight))
                ));
        }
    }
}
