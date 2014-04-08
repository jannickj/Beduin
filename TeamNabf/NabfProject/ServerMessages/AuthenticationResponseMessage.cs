using NabfProject.ServerMessages;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml.Linq;

namespace NabfProject.ServerMessages
{
    public class AuthenticationResponseMessage : InternalReceiveMessage
    {
        
        public ServerResponseTypes Response { get; private set; }

        public override void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();
            if("ok".Equals(reader["result"]))
                Response = ServerResponseTypes.Success;
            reader.Read();
        }
    }
}
