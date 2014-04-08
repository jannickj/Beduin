using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class SurveyedEdgeMessage : InternalReceiveMessage
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
        private int weight;

        public int Weight
        {
            get { return weight; }
        }

        public override void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();

            node1 = reader["node1"];
            node2 = reader["node2"];
            weight = Convert.ToInt32(reader["weight"]);
        }
    }
}
