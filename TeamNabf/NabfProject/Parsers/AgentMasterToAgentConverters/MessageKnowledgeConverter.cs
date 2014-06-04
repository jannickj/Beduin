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
    public class MessageKnowledgeConverter : JSConverter<MessageKnowledge, IilElement>
    {
        public override object ForeignID
        {
            get
            {
                return "messageKnowledge";
            }
        }

        public override MessageKnowledge BeginConversionToKnown(IilElement fobj)
        {
            var ia = (IilFunction)fobj;

            var identifier1 = (IilIdentifier)ia.Parameters[0];
            var identifier2 = (IilIdentifier)ia.Parameters[1];

            MessageKnowledge rlk = new MessageKnowledge(identifier1.Value, identifier2.Value);

            return rlk;
        }

        public override IilElement BeginConversionToForeign(MessageKnowledge gobj)
        {
            return new IilPerceptCollection(new IilPercept("message"
                    , new IilFunction("message"
                        , new IilFunction("recipient", new IilIdentifier(gobj.TargetedAgent))
                        , new IilFunction("message", new IilIdentifier(gobj.Message)))
                    ));
        }
    }
}
