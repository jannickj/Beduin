using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Xml;
using System.Xml.Serialization;

namespace NabfProject.AI
{
    [XmlRoot("packet")]
    public class XmlTransmitterMessage<Data>
        where Data : IXmlSerializable
    {
        private Data message;

        public Data Message
        {
            get { return message; }
        }

		public virtual bool UseSeralizerOnMsg
		{
			get
			{
				return true;
			}
		}

        public XmlTransmitterMessage()
        {

        }

        public XmlTransmitterMessage(Data message)
        {
            this.message = message;
        }

        public void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();
            if (!reader.IsStartElement())
                reader.ReadEndElement();
            this.ReadNode(reader);
            reader.ReadStartElement(NodeName);
            this.message = this.ConstructMessage(reader);
            message.ReadXml(reader);
        }

        public void WriteXml(XmlWriter writer, XmlSerializer serializer)
        {
            writer.WriteStartElement(NodeName);
			if (this.UseSeralizerOnMsg)
//				serializer.Serialize(writer, message);
				message.WriteXml (writer);
			else
			{
				BeforeWriteMessage(writer);
				message.WriteXml(writer);
			}
            writer.WriteEndElement();
        }

		public virtual void BeforeWriteMessage(XmlWriter writer)
		{

		}

        public virtual Data ConstructMessage(XmlReader reader)
        {
            return (Data) Activator.CreateInstance(typeof(Data));
        }

        public virtual void ReadNode(XmlReader reader)
        {

        }

        public virtual string NodeName
        {
            get
            {
                return "packet";
            }
        }
    }
}
