using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace NabfProject.KnowledgeManagerModel
{
    public class HeuristicKnowledge : Knowledge
    {
        public string Node1 { get; private set; }
        public string Node2 { get; private set; }
        public int Distance { get; private set;}

        public HeuristicKnowledge(string node1, string node2, int distance)
        {
            Node1 = node1;
            Node2 = node2;
            Distance = distance;
        }
		public override bool Equals(object obj)
		{
			if (obj is Knowledge)
				return this.Equals((Knowledge)obj);
			else
				return false;
		}

		public override int GetHashCode()
		{
			return this.Node1.GetHashCode() + this.Node2.GetHashCode();
		}

		public bool Equals(Knowledge other)
        {
            if (other == null)
                throw new ArgumentException("Input of method Equals in " + this.GetType().Name + " is null");
            else if (!(other is Knowledge))
                throw new ArgumentException("Object : " + other.GetType().Name + " of Equals in " + other.GetType().Name + " is not implementing interface Knowledge");

            if (other.GetType() != this.GetType())
                return false;

            HeuristicKnowledge hk = (HeuristicKnowledge)other;

			return (hk.Node1 == this.Node1 && hk.Node2 == this.Node2);
        }

        int IComparable<Knowledge>.CompareTo(Knowledge other)
        {
            if (other == null)
                throw new ArgumentException("Input of method CompareTo in " + this.GetType().Name + " is null");
            else if (other is HeuristicKnowledge)
                return -1;
            else
                throw new ArgumentException("Object : " + other.GetType().Name + " of CompareTo in " + other.GetType().Name + " is not of type HeuristicKnowledge");
        }

        public string GetTypeToString()
        {
            return "heuristicKnowledge";
        }
    }
}
