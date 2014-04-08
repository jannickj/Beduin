using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class SimulationMessage : InternalReceiveMessage
    {
        private int step;
        private string messageName;

        public string MessageName
        {
            get { return messageName; }
        }

        public int Step
        {
            get { return step; }
        }
        public override void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();

            step = Convert.ToInt32(reader["step"]);
            messageName = reader.LocalName;
            if (reader.IsEmptyElement)
            {
                reader.Read();
                reader.MoveToContent();
            }
        }
    }
}
