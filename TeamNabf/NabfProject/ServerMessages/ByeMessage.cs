using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class ByeMessage : InternalReceiveMessage
    {
        public ServerResponseTypes Response { get; private set; }

        public override void ReadXml(System.Xml.XmlReader reader)
        {
            Response = ServerResponseTypes.Success;
        }
    }
}
