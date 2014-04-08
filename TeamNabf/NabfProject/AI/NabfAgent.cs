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
    }
}
