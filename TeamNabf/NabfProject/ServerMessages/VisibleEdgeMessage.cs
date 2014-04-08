using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class VisibleEdgeMessage : InternalReceiveMessage
    {
        private string node1;

        public string Node1
        {
            get { return node1; }
        }
        private string node2;

        public string Node2
        {
            get { return node2; }
        }

        public override void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();

            node1 = reader["node1"];
            node2 = reader["node2"];
        }
    }
}
