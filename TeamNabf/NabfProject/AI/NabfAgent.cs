using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace NabfProject.AI
{
    public class NabfAgent : XmasEngineModel.EntityLib.Agent, IEqualityComparer<NabfAgent>, IEquatable<NabfAgent>
    {
        public long Id { get; private set; }
        public bool GotJobThisRound = false;
        public Int64 IdOfLastJob = -1;

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

        /// <summary>
        /// Compares two agents using their (unique) names
        /// </summary>
        /// <param name="obj">agent to compare with. If null or not of type NabfAgent this will return false. </param>
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

        bool IEqualityComparer<NabfAgent>.Equals(NabfAgent x, NabfAgent y)
        {
            return x.Name == y.Name;
        }

        int IEqualityComparer<NabfAgent>.GetHashCode(NabfAgent obj)
        {
            return obj.Name.GetHashCode();
        }

        bool IEquatable<NabfAgent>.Equals(NabfAgent other)
        {
            return this.Name == other.Name;
        }
    }
}
