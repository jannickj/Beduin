using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class ActionMessage : InternalSendMessage
    {
        private string actionType;
        private string actionParam = "";
        private int id;

        public ActionMessage(int id, string actionType) : this(id, actionType, "")
        {
        }

        public ActionMessage(int id, string actionType, string actionParam)
        {
            this.actionType = actionType;
            this.actionParam = actionParam;
            this.id = id;
        }

        protected override string MessageType
        {
            get { return "action"; }
        }

        public override void ReadXml(System.Xml.XmlReader reader)
        {
            throw new NotImplementedException();
        }

        protected override void WriteXmlInternal(System.Xml.XmlWriter writer)
        {
            writer.WriteStartElement("action");
            writer.WriteAttributeString("id", Convert.ToString(id));
            writer.WriteAttributeString("type", actionType);
            if  ("" != actionParam)
                writer.WriteAttributeString("param", actionParam);
            writer.WriteEndElement();
        }

        public override string ToString()
        {
            var param = "";
            if (!String.IsNullOrEmpty(actionParam))
                param = ", " + actionParam;
            return actionType+"("+id+param+")";
        }
    }
}
