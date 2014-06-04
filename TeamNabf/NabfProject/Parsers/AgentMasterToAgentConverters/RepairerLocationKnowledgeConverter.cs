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
    public class RepairerLocationKnowledgeConverter : JSConverter<RepairerLocationKnowledge, IilElement>
    {
        public override object ForeignID
        {
            get
            {
                return "repairerLocationKnowledge";
            }
        }

        public override RepairerLocationKnowledge BeginConversionToKnown(IilElement fobj)
        {
            var ia = (IilFunction)fobj;

            var identifier1 = (IilIdentifier)ia.Parameters[0];
            var identifier2 = (IilIdentifier)ia.Parameters[1];

            RepairerLocationKnowledge rlk = new RepairerLocationKnowledge(identifier1.Value, identifier2.Value);

            return rlk;
        }

        public override IilElement BeginConversionToForeign(RepairerLocationKnowledge gobj)
        {
            return new IilPerceptCollection(new IilPercept("repairerLocations"
                    , new IilFunction("repairerLocation"
                        , new IilFunction("agentToBeRepaired", new IilIdentifier(gobj.TargetedAgent))
                        , new IilFunction("nodeOfRepairer", new IilIdentifier(gobj.NodeOfRepairer)))
                    ));
        }
    }
}
