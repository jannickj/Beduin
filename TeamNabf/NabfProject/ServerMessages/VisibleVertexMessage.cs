using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class VisibleVertexMessage : InternalReceiveMessage
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

        public override void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();

            name = reader["name"];
            team = reader["team"];
        }
    }
}
