using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class SimEndMessage : InternalReceiveMessage
    {
		public int Ranking { get; private set; }
		public int Score { get; private set; }

        public override void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();

            string ranking = reader["ranking"];
			if (ranking != null && ranking != "")
				this.Ranking = Convert.ToInt32(ranking);

            string score = reader["score"];
			if (score != null && score != "")
				this.Score = Convert.ToInt32(score);
                       
            reader.Read();
        }

        
    }
}
