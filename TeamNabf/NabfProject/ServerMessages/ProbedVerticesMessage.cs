using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class ProbedVerticesMessage : InternalReceiveMessage
    {
        private List<InternalReceiveMessage> probedVertices = new List<InternalReceiveMessage>();

        public List<InternalReceiveMessage> ProbedVertices
        {
            get { return probedVertices; }
        }

        private string messageName;

        public string MessageName
        {
            get { return messageName; }
        }

        public override void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();
            messageName = reader.LocalName;
            if (reader.IsEmptyElement)
            {
                reader.Read();
                reader.MoveToContent();
                return;
            }
            reader.Read();
            reader.MoveToContent();

            while (reader.LocalName != "probedVertices")
            {
                reader.MoveToContent();
                var message = ServerMessageFactory.Instance.ConstructMessage(reader.LocalName);
                message.ReadXml(reader);
                probedVertices.Add(message);
                reader.Read();
                reader.MoveToContent();
            }
            if (reader.IsEmptyElement)
            {
                reader.Read();
                reader.MoveToContent();
            }
            else
            {
                reader.ReadEndElement();
                reader.MoveToContent();
            }
        }
    }
}
