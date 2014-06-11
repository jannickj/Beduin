using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace NabfProject.AI
{
    public class NabfAgent : XmasEngineModel.EntityLib.Agent
    {
        public long Id { get; private set; }

        public NabfAgent(string s)
            : base(s)
        {

            var match = Regex.Match(s, "([0-9]+)");
            if (match.Success)
            {               
                Id = Convert.ToInt64(match.Value);
            }
            else
            {
                Random rng = new Random();
                Id = rng.Next(Int32.MaxValue - 1);
            }
        }

        public override bool Equals(object obj)
        {
            if (obj == null)
                return false;
            else if (!(obj is NabfAgent))
                return false;
            return this.Name == ((NabfAgent)obj).Name;
        }

        public override string ToString()
        {
            return Name;
        }
    }
}
