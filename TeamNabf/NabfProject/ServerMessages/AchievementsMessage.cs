using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class AchievementsMessage : InternalReceiveMessage
    {
        private List<InternalReceiveMessage> achievementList = new List<InternalReceiveMessage>();

        public List<InternalReceiveMessage> AchievementList
        {
            get { return achievementList; }
        }

        public override void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();
            if (reader.IsEmptyElement)
            {
                reader.Read();
                reader.MoveToContent();
                return;
            }
            reader.Read();
            reader.MoveToContent();

            while (reader.LocalName != "achievements")
            {
                reader.MoveToContent();
                var message = ServerMessageFactory.Instance.ConstructMessage(reader.LocalName);
                message.ReadXml(reader);
                achievementList.Add(message);
                reader.Read();
                reader.MoveToContent();
            }
            reader.ReadEndElement();
            reader.MoveToContent();
        }
    }
}
