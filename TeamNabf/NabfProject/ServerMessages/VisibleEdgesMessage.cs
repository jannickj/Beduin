using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class VisibleEdgesMessage : InternalReceiveMessage
    {
        private List<InternalReceiveMessage> visibleEdges = new List<InternalReceiveMessage>();

        public List<InternalReceiveMessage> VisibleEdges
        {
            get { return visibleEdges; }
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

            while (reader.LocalName != "visibleEdges")
            {
                reader.MoveToContent();
                var message = ServerMessageFactory.Instance.ConstructMessage(reader.LocalName);
                message.ReadXml(reader);
                visibleEdges.Add(message);
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
