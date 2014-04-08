using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class PerceptionMessage : InternalReceiveMessage
    {
        private List<InternalReceiveMessage> elements = new List<InternalReceiveMessage>();
        private long deadline;
        private int id;
        private string messageName;

        public string MessageName
        {
            get { return messageName; }
        }

        public List<InternalReceiveMessage> Elements { get; private set; }
        public long Deadline { get; private set; }
        public int Id { get; private set; }

        public override void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();
            deadline = Convert.ToInt64(reader["deadline"]);
            id = Convert.ToInt32(reader["id"]);
            messageName = reader.LocalName;
            reader.Read();
            reader.MoveToContent();

            while (reader.LocalName != "perception")
            {
                reader.MoveToContent();
                var message = ServerMessageFactory.Instance.ConstructMessage(reader.LocalName);
                message.ReadXml(reader);
                elements.Add(message);
            } 
            reader.ReadEndElement();

            Elements = elements;
            Deadline = deadline;
            Id = id;
        }


    }
}
