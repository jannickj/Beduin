using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class AchievementMessage : InternalReceiveMessage
    {
        private string name;

        public string Name
        {
            get { return name; }
        }
        public override void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();

            name = reader["name"];
        }
    }
}
