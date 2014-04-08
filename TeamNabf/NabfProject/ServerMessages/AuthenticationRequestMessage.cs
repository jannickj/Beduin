using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class AuthenticationRequestMessage : InternalSendMessage
    {
        private string username;
        private string password;

        public AuthenticationRequestMessage(string username, string password)
        {
            this.username = username;
            this.password = password;
        }

        public override void ReadXml(System.Xml.XmlReader reader)
        {
            throw new NotImplementedException();
        }

        protected override string MessageType
        {
            get { return "auth-request"; }
        }

        protected override void WriteXmlInternal(System.Xml.XmlWriter writer)
        {
            writer.WriteStartElement("authentication");
            writer.WriteAttributeString("password", password);
            writer.WriteAttributeString("username", username);
            writer.WriteEndElement();
        }
    }
}
