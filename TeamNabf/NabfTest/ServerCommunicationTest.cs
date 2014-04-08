using NabfProject.AI;
using NabfProject.ServerMessages;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfTest
{
    [TestFixture]
    class ServerCommunicationTest
    {
        [Test]
        public void BeforeDeserialize_OneZeroTerminatedMessage_ParsedMessage()
        {
            MemoryStream memStream = new MemoryStream();
            StreamReader reader = new StreamReader(memStream);
            ServerCommunication servCom = new ServerCommunication(reader, null);
            StreamWriter swriter = new StreamWriter(memStream);
            int timeStamp = 1;

            swriter.Write(GenerateXmlMessage(timeStamp));

            swriter.Flush();

            memStream.WriteByte(0);

            memStream.Position = 0;

            var message = (ReceiveMessage) servCom.DeserializePacket();

            Assert.IsTrue(matchGeneratedMessage(message, timeStamp));
        }

        [Test]
        public void BeforeDeserialize_OneAndHalfZeroTerminatedMessage_ParsedMessage()
        {
            MemoryStream memStream = new MemoryStream();
            StreamReader reader = new StreamReader(memStream);
            ServerCommunication servCom = new ServerCommunication(reader, null);
            StreamWriter swriter = new StreamWriter(memStream);
            int timeStamp = 1;
            int timeStamp2 = 2;

            swriter.Write(GenerateXmlMessage(timeStamp));

            swriter.Flush();

            memStream.WriteByte(0);

            string halfMsg = GenerateXmlMessage(timeStamp2).Substring(0, 60);
            swriter.Write(halfMsg);
            swriter.Flush();
            memStream.Position = 0;

            var message = (ReceiveMessage)servCom.DeserializePacket();
            var curPos = memStream.Position;

            Assert.IsTrue(matchGeneratedMessage(message, timeStamp));

            string half2Msg = GenerateXmlMessage(timeStamp2).Substring(60);

            memStream.Position = memStream.Length;            

            swriter.Write(half2Msg);
            swriter.Flush();
            memStream.WriteByte(0);
            memStream.Position = curPos;

            var message2 = (ReceiveMessage)servCom.DeserializePacket();

            Assert.IsTrue(matchGeneratedMessage(message2, timeStamp2));

            

            
        }

        [Test]
        public void BeforeDeserialize_2andHalfZeroTerminatedMessage_ParsedMessage()
        {
            MemoryStream memStream = new MemoryStream();
            StreamReader reader = new StreamReader(memStream);
            ServerCommunication servCom = new ServerCommunication(reader, null);
            StreamWriter swriter = new StreamWriter(memStream);
            int timeStamp = 1;
            int timeStamp2 = 2;
            int timeStamp3 = 3;

            swriter.Write(GenerateXmlMessage(timeStamp));

            swriter.Flush();

            memStream.WriteByte(0);

            swriter.Write(GenerateXmlMessage(timeStamp2));
            swriter.Flush();
            memStream.WriteByte(0);

            string halfMsg = GenerateXmlMessage(3).Substring(0, 60);
            swriter.Write(halfMsg);
            swriter.Flush();
            memStream.Position = 0;

            var message = (ReceiveMessage)servCom.DeserializePacket();
            var curPos = memStream.Position;

            Assert.IsTrue(matchGeneratedMessage(message, timeStamp));

            string half2Msg = GenerateXmlMessage(timeStamp3).Substring(60);

            memStream.Position = memStream.Length;

            swriter.Write(half2Msg);
            swriter.Flush();
            memStream.WriteByte(0);
            memStream.Position = curPos;

            var message2 = (ReceiveMessage)servCom.DeserializePacket();

            Assert.IsTrue(matchGeneratedMessage(message2, timeStamp2));

            var message3 = (ReceiveMessage)servCom.DeserializePacket();

            Assert.IsTrue(matchGeneratedMessage(message3, timeStamp3));

        }

        private string GenerateXmlMessage(int timeStamp)
        {
            string result = "ok";
            return "<message timestamp=\""+timeStamp+"\" type=\"auth-response\">"
                            + "<authentication result=\"" + result + "\"/>"
                            + "</message>";
        }

        private bool matchGeneratedMessage(ReceiveMessage genMessage, int timeStamp)
        {
            return genMessage.Timestamp == timeStamp && ((AuthenticationResponseMessage)genMessage.Message).Response == ServerResponseTypes.Success;
        }
    }
}
