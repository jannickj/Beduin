using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class SimStartMessage : InternalReceiveMessage
    {

		public int Edges { get; private set; }
		public int Id { get; private set; }
		public int Steps { get; private set; }
		public int Vertices { get; private set; }
		public string Role { get; private set; }


        public override void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();

            string edges = reader["edges"];
			if (edges != null && edges != "")
				this.Edges = Convert.ToInt32(edges);

            string id = reader["id"];
			if (id != null && id != "")
				this.Id = Convert.ToInt32(id);

            string steps = reader["steps"];
			if (steps != null && steps != "")
				this.Steps = Convert.ToInt32(steps);

            string vertices = reader["vertices"];
			if (vertices != null && vertices != "")
				this.Vertices = Convert.ToInt32(vertices);

            string role = reader["role"];
			if (role != null && role != "")
				this.Role = role;

            reader.Read();
            reader.MoveToContent();
        }

        
    }
}
