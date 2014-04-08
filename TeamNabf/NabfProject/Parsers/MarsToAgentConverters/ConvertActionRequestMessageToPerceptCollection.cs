using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.Conversion;
using JSLibrary.IiLang;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;
using NabfProject.ServerMessages;

namespace NabfProject.Parsers.MarsToAgentConverters
{
    public class ConvertActionRequestMessageToPerceptCollection : JSConverterToForeign<ReceiveMessage,IilPerceptCollection>
    {
        public override object KnownID
        {
            get
            {
                return typeof(RequestActionMessage);
            }
        }

        public override IilPerceptCollection BeginConversionToForeign(ReceiveMessage gobj)
        {
            RequestActionMessage ram = (RequestActionMessage)gobj.Message;
            PerceptionMessage pm = (PerceptionMessage)ram.Response;

            List<IilPercept> ipl = new List<IilPercept>(){new IilPercept("actionRequest", new IilFunction("id", new IilNumeral(pm.Id))
                , new IilFunction("deadline", new IilNumeral(pm.Deadline)), new IilFunction("timestamp", new IilNumeral(gobj.Timestamp)))};

            foreach(IilPercept ip in ((IilPerceptCollection)this.ConvertToForeign(ram.Response)).Percepts)
                ipl.Add(ip);

            IilPerceptCollection ipc = new IilPerceptCollection(ipl.ToArray());

            return ipc;
        }
    }
}
