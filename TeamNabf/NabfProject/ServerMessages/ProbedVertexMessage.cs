using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class ProbedVertexMessage : InternalReceiveMessage
    {
        private string name;

        public string Name
        {
            get { return name; }
        }
        private int value;

        public int Value
        {
            get { return this.value; }
        }

        public override void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();

            name = reader["name"];
            value = Convert.ToInt32(reader["value"]);
        }
    }
}
