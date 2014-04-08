using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class RequestActionMessage : InternalReceiveMessage
    {
        public InternalReceiveMessage Response { get; private set; }
        public override void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();
            var message = ServerMessageFactory.Instance.ConstructMessage(reader.LocalName);
            message.ReadXml(reader);
            Response = message;
        }

    }
}
