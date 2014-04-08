using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Serialization;

namespace NabfProject.ServerMessages
{
    public abstract class InternalReceiveMessage : IXmlSerializable
    {       
        public System.Xml.Schema.XmlSchema GetSchema()
        {
            throw new NotImplementedException();
        }

        public abstract void ReadXml(System.Xml.XmlReader reader);

        public void WriteXml(System.Xml.XmlWriter writer)
        {
            throw new NotImplementedException();
        }

        
    }
}
