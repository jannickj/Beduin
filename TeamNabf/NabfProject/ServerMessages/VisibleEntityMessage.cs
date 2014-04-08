using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class VisibleEntityMessage : InternalReceiveMessage
    {
        private string name;

        public string Name
        {
            get { return name; }
        }
        private string team;

        public string Team
        {
            get { return team; }
        }
        private string node;

        public string Node
        {
            get { return node; }
        }
        private string status;

        public string Status
        {
            get { return status; }
        }

        public override void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();

            name = reader["name"];
            team = reader["team"];
            node = reader["node"];
            status = reader["status"];
        }
    }
}
