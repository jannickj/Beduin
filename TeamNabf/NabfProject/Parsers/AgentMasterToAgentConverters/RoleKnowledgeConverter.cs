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
    public class RoleKnowledgeConverter : JSConverter<RoleKnowledge, IilElement>
    {
        public override object ForeignID
        {
            get
            {
                return "roleKnowledge";
            }
        }

        public override RoleKnowledge BeginConversionToKnown(IilElement fobj)
        {
            //<IilAction name="roleKnowledge" >
            //  <IilIdentifier agentId="?" />
            //  <IilIdentifier role="?" />
            //  <IilNumeral sureness=? />
            //</ IilAction>
            var ia = (IilFunction)fobj;

            var identifier1 = (IilIdentifier)ia.Parameters[0];
            var identifier2 = (IilIdentifier)ia.Parameters[1];
            var numeral = (IilNumeral)ia.Parameters[2];

            RoleKnowledge rk = new RoleKnowledge(identifier1.Value, identifier2.Value, (int)numeral.Value);

            return rk;
        }

        public override IilElement BeginConversionToForeign(RoleKnowledge gobj)
        {
            return new IilPerceptCollection(new IilPercept("roleKnowledge"
                , new IilFunction("role", new IilIdentifier(gobj.Role))
                , new IilFunction("agentId", new IilIdentifier(gobj.AgentId))
                , new IilFunction("sureness", new IilNumeral(gobj.Sureness))
                ));
        }
    }
}
