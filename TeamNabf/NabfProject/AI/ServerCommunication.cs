using NabfProject.Library;
using NabfProject.ServerMessages;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using System.Xml.Serialization;

namespace NabfProject.AI
{
    public class ServerCommunication : XmlPacketTransmitter<InternalReceiveMessage, InternalSendMessage>
    {              
        public ServerCommunication(StreamReader reader, StreamWriter writer) : base(reader, writer) { }
        private int bufferSize = 4096;
        private byte[] buffer;
        private MemoryStream store = new MemoryStream();
        StreamSplitter streamSplitter;
        Stream debugStream;

        //public void SendMessage(ServerMessages.SendMessage message)
        //{
        //    stream.WriteStartDocument(false);
        //    xmlSerSend.Serialize(stream, message);
        //    stream.Flush();
        //}

        //public ServerMessages.ReceiveMessage ReceiveMessage()
        //{
        //    if (reader == null)
        //        reader = XmlReader.Create(this.TestStream);
        //    return (ReceiveMessage) xmlSerReceive.Deserialize(reader);

        //}

        protected override XmlTransmitterMessage<InternalReceiveMessage> ConstrutReceiverMessage()
        {
            return new ReceiveMessage();
        }

        protected override XmlTransmitterMessage<InternalSendMessage> ConstrutSenderMessage(InternalSendMessage data)
        {
            return new SendMessage(data);
        }

        public override void BeforeDeserialize(XmlReader reader, StreamReader sreader)
        {
            MemoryStream memstream = new MemoryStream();
            MemoryStream tempStream = new MemoryStream();
            Stream stream = sreader.BaseStream;
            debugStream = new MemoryStream();
            streamSplitter = new StreamSplitter(stream, debugStream);
            buffer = new byte[bufferSize];
            int bytesRead = -1;
            int data = -1;
            bool remainingDataInBuffer = false;
            int storeIterator = -1;
            int bufferIterator = -1;
            bool remainingDataStored = false;
            bool messageStored = store.Length != 0;
            bool incompleteMessageInStore = true;
            bool incompleteMessageInBuffer = true;
            bool debug = false;



            if (messageStored) //Message stored, append data from stream to store and read from store 
            {
                ReadFromStoreToMemstream(memstream, ref data, ref storeIterator, ref remainingDataStored, ref incompleteMessageInStore);

                //While message not complete
                if (incompleteMessageInStore)
                {
                    while (incompleteMessageInBuffer)
                    {
                        ReadFromStreamToBuffer(stream, ref bytesRead);

                        ReadFromBufferToMemstream(memstream, bytesRead, ref data, ref remainingDataInBuffer, ref bufferIterator, ref incompleteMessageInBuffer);
                    }

                    //If something left in buffer
                    if (remainingDataInBuffer)
                    {
                        StashBufferInStore(bytesRead, ref bufferIterator);
                    }
                }

                //Move content in store to front
                if (remainingDataStored)
                {
                    MoveStoreContentToFront(tempStream, ref data, ref storeIterator);

                }
                else
                    store = new MemoryStream();
                                        
            }
            else if (!messageStored) //No message stored, read directly from stream.
            {
                //While message not ended
                while (incompleteMessageInBuffer)
                {
                    ReadFromStreamToBuffer(stream, ref bytesRead);

                    ReadFromBufferToMemstream(memstream, bytesRead, ref data, ref remainingDataInBuffer, ref bufferIterator, ref incompleteMessageInBuffer);
                }
                ///If something left in buffer
                if (remainingDataInBuffer)
                {
                    StashBufferInStore(bytesRead, ref bufferIterator);
                }

            }         

            //Simple yet ineffective version

            //do
            //{
            //    data = sreader.BaseStream.ReadByte();
            //    if (data == 0)
            //        break;
            //    else
            //        memstream.WriteByte((byte)data);
            //} while (true);

            if (debug)
            {
                string agent = "Nabf1";
                FileStream fs = new FileStream("Debug/debugXmlMessages"+agent+".txt", FileMode.Append);
                                
                byte[] buf = new byte[8192];
                debugStream.Position = 0;

                for (; ; )
                {
                    int numRead = debugStream.Read(buf, 0, buf.Length);
                    if (numRead == 0)
                        break;
                    fs.Write(buf, 0, numRead);
                }
                fs.Close();
            }

            memstream.Position = 0;
            this.ChangeReader(XmlReader.Create(memstream, new XmlReaderSettings() { ConformanceLevel = ConformanceLevel.Fragment }));

        }

        private void MoveStoreContentToFront(MemoryStream tempStream, ref int data, ref int storeIterator)
        {
            for (; storeIterator < store.Length; storeIterator++)
            {
                data = store.ReadByte();
                tempStream.WriteByte((byte)data);
            }
            store = tempStream;
        }

        private void StashBufferInStore(int bytesRead, ref int bufferIterator)
        {
            //Stash buffer in store
            for (; bufferIterator < bytesRead; bufferIterator++)
            {
                var value = buffer[bufferIterator];
                store.WriteByte(value);
            }
        }

        private void ReadFromBufferToMemstream(MemoryStream memstream, int bytesRead, ref int data, ref bool remainingDataInBuffer, ref int bufferIterator, ref bool incompleteMessageInBuffer)
        {
            //Read from buffer to memstream
            for (bufferIterator = 0; bufferIterator < bytesRead; bufferIterator++)
            {
                data = buffer[bufferIterator];

                if (data != 0)
                    memstream.WriteByte((byte)data);
                else
                {
                    incompleteMessageInBuffer = false;
                    if (bytesRead > ++bufferIterator)
                        remainingDataInBuffer = true;
                    break;
                }

            }
        }

        private void ReadFromStreamToBuffer(Stream stream, ref int bytesRead)
        {
            //Read from stream to buffer
            buffer = new byte[bufferSize];
            bytesRead = streamSplitter.Read(buffer, 0, buffer.Length);

            double time = (DateTime.Now - new DateTime(1970, 1, 1)).TotalMilliseconds - 3600000;
            char[] timeS = time.ToString().ToCharArray();
            for (int i = 0; i < timeS.Length; i++)
            {
                var digit = Convert.ToByte(timeS[i]);

                debugStream.WriteByte(digit);
            }
            
            if (bytesRead == 0)
                throw new Exception("Would block if TCP");
        }

        private void ReadFromStoreToMemstream(MemoryStream memstream, ref int data, ref int storeIterator, ref bool remainingStored, ref bool incompleteMessageInStore)
        {
            //Read message from store to memstream
            store.Position = 0;
            for (storeIterator = 0; storeIterator < store.Length; storeIterator++)
            {
                data = store.ReadByte();
                if (data != 0)
                    memstream.WriteByte((byte)data);
                else
                {
                    incompleteMessageInStore = false;
                    if (store.Length > ++storeIterator)
                        remainingStored = true;
                    break;
                }
            }
        }
               
		public override void AfterDeserialize(XmlReader reader, StreamReader sreader)
		{
            
			//this.ChangeReader(XmlReader.Create(sreader, new XmlReaderSettings() { ConformanceLevel = ConformanceLevel.Fragment }));
		}


        public override void BeforeSerialize(XmlWriter writer, StreamWriter swriter, XmlTransmitterMessage<InternalSendMessage> packet)
        {
            //swriter.Write("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>");
        }

        public override void AfterSerialize(XmlWriter writer, StreamWriter swriter, XmlTransmitterMessage<InternalSendMessage> packet)
        {
            swriter.BaseStream.WriteByte(0);
            swriter.BaseStream.Flush();
        }
	
    }
}
