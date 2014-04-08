using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class VisibleVerticesMessage : InternalReceiveMessage
    {
        private List<InternalReceiveMessage> visibleVertices = new List<InternalReceiveMessage>();

        public List<InternalReceiveMessage> VisibleVertices
        {
            get { return visibleVertices; }
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

            while (reader.LocalName != "visibleVertices")
            {
                reader.MoveToContent();
                var message = ServerMessageFactory.Instance.ConstructMessage(reader.LocalName);
                message.ReadXml(reader);
                visibleVertices.Add(message);
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
