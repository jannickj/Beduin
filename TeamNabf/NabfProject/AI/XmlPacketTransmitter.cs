using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using System.Xml.Serialization;
using JSLibrary.IiLang;
using JSLibrary.IiLang.DataContainers;
using System.IO;

namespace NabfProject.AI
{
    
	public class XmlPacketTransmitter<TRecieve,TSend> 
		where TRecieve : IXmlSerializable
		where TSend : IXmlSerializable
	{
        private StreamReader reader;
        private StreamWriter writer;
//		private XmlSerializer serializerReciever;
//		private XmlSerializer serializerSender;
        private XmlReader xreader;
        private XmlWriter xwriter;

        public XmlPacketTransmitter(StreamReader reader, StreamWriter writer)
		{
			
			this.reader = reader;
			this.writer = writer;
//			serializerReciever = new XmlSerializer(typeof(TRecieve));
//			serializerSender = new XmlSerializer(typeof(TSend));
		}

        protected virtual XmlTransmitterMessage<TRecieve> ConstrutReceiverMessage()
        {
            return new XmlTransmitterMessage<TRecieve>();
        }

        protected virtual XmlTransmitterMessage<TSend> ConstrutSenderMessage(TSend data)
        {
            return new XmlTransmitterMessage<TSend>(data);
        }


		public XmlTransmitterMessage<TRecieve> DeserializePacket()
		{
            
            
            this.BeforeDeserialize(xreader, reader);
            var msg = this.ConstrutReceiverMessage();
            msg.ReadXml(xreader);
			this.AfterDeserialize(xreader, reader);
			return msg;

		}

        public TRecieve DeserializeMessage()
        {
            var msg =  this.DeserializePacket().Message;
			
			return msg;
        }

		public void SeralizePacket(TSend action)
		{
            SeralizePacket(this.ConstrutSenderMessage(action));
		}

        public void SeralizePacket(XmlTransmitterMessage<TSend> packet)
        {
            if (xwriter == null)
                xwriter = XmlWriter.Create(writer, new XmlWriterSettings() { ConformanceLevel = System.Xml.ConformanceLevel.Fragment });

			BeforeSerialize(xwriter, this.writer, packet);
			packet.WriteXml(this.xwriter,null);
            this.xwriter.Flush();
			AfterSerialize(xwriter, this.writer, packet);
            
        }


		protected void ChangeReader(XmlReader xmlReader)
		{
			this.xreader = xmlReader;
		}

		public virtual void BeforeDeserialize(XmlReader reader, StreamReader sreader)
		{
            if (xreader == null)
                xreader = XmlReader.Create(this.reader, new XmlReaderSettings() { ConformanceLevel = ConformanceLevel.Fragment });
		}

		public virtual void AfterDeserialize(XmlReader reader, StreamReader sreader)
		{

		}

		public virtual void BeforeSerialize(XmlWriter writer, StreamWriter swriter, XmlTransmitterMessage<TSend> packet)
		{

		}

		public virtual void AfterSerialize(XmlWriter writer, StreamWriter swriter, XmlTransmitterMessage<TSend> packet)
		{

		}
	}
}
