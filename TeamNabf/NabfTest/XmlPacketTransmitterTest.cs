using JSLibrary.IiLang;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;
using NabfProject.AI;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Xml;

namespace NabfTest
{
    [TestFixture]
    public class XmlPacketTransmitterTest
    {
        private object locker = new Object();
        
        //Scenario Test Not Meant To be executed
        //[Test]
        public void SendPackage_OverTcp_PacketReceived()
        {
            
            TcpListener lister = new TcpListener(IPAddress.Parse("127.0.0.1"),1337);
            lister.Start();
            new Thread(new ThreadStart(Client)).Start();
            var client = lister.AcceptTcpClient();
            StreamReader sreader = new StreamReader(client.GetStream(), Encoding.UTF8);
            StreamWriter swriter = new StreamWriter(client.GetStream(), Encoding.UTF8);

            XmlPacketTransmitter<IilAction, IilPerceptCollection> receiver = new XmlPacketTransmitter<IilAction, IilPerceptCollection>(sreader, swriter);
            IilAction action = receiver.DeserializeMessage();
            IilAction action2 = receiver.DeserializeMessage();
            Assert.Pass();
        }

        private void Client()
        {
            var client = new TcpClient();
            client.Connect("127.0.0.1", 1337);
            StreamReader sreader = new StreamReader(client.GetStream(), Encoding.UTF8);
            StreamWriter swriter = new StreamWriter(client.GetStream(), Encoding.UTF8);

            XmlPacketTransmitter<IilPerceptCollection,IilAction> sender = new XmlPacketTransmitter<IilPerceptCollection,IilAction>(sreader, swriter);
            IilAction data = new IilAction("test", new IilFunction("val", new IilNumeral(2)));
            IilAction data2 = new IilAction("test2", new IilFunction("val2", new IilNumeral(2)));
            sender.SeralizePacket(data);
            sender.SeralizePacket(data);
        }
    }
}
