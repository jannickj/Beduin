using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace NabfProject.KnowledgeManagerModel
{
    public class EdgeKnowledge : Knowledge
    {
        public string Node1 { get; private set; }
        public string Node2 { get; private set; }
        public int Weight { get; private set;}

        public EdgeKnowledge(string node1, string node2, int weight)
        {
            Node1 = node1;
            Node2 = node2;
            Weight = weight;
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
                throw new ArgumentException("Input of Equals of " + this.GetType().Name + " is null");
            else if (!(other is Knowledge))
                throw new ArgumentException("Object : " + other.GetType().Name + " of Equals is not implementing interface Knowledge");

            if (other.GetType() != this.GetType())
                return false;

            EdgeKnowledge ek = (EdgeKnowledge)other;

			return (ek.Node1 == this.Node1 && ek.Node2 == this.Node2) || (ek.Node1 == this.Node2 && ek.Node2 == this.Node1);
        }

        int IComparable<Knowledge>.CompareTo(Knowledge other)
        {
            if (other == null)
                throw new ArgumentException("Input of CompareTo of " + this.GetType().Name + " is null");
            else if (other is EdgeKnowledge)
                if (((EdgeKnowledge)other).Weight < Weight)
                    return -1;
                else if (((EdgeKnowledge)other).Weight > Weight)
                    return 1;
                else
                    return 0;
            else
                throw new ArgumentException("Object : " + other.GetType().Name + " of CompareTo is not of type EdgeKnowledge");
        }

        public string GetTypeToString()
        {
            return "edgeKnowledge";
        }
    }
}
