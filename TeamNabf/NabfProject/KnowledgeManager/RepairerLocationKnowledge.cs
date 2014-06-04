using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace NabfProject.KnowledgeManagerModel
{
    public class RepairerLocationKnowledge : TargetedKnowledge
    {
        //public string Repairer { get; private set; }
        public string TargetedAgent { get; private set; }
        public string NodeOfRepairer { get; private set;}

        public RepairerLocationKnowledge(string repairee, string nodeOfRepairer)
        {
            TargetedAgent = repairee;
            NodeOfRepairer = nodeOfRepairer;
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
            return this.TargetedAgent.GetHashCode() + this.NodeOfRepairer.GetHashCode();
		}

		public bool Equals(Knowledge other)
        {
            if (other == null)
                throw new ArgumentException("Input of method Equals in " + this.GetType().Name + " is null");
            else if (!(other is Knowledge))
                throw new ArgumentException("Object : " + other.GetType().Name + " of Equals is not implementing interface Knowledge");

            if (other.GetType() != this.GetType())
                return false;

            RepairerLocationKnowledge rlk = (RepairerLocationKnowledge)other;

			return (rlk.NodeOfRepairer == this.NodeOfRepairer && rlk.TargetedAgent == this.TargetedAgent);
        }

        int IComparable<Knowledge>.CompareTo(Knowledge other)
        {
            if (other == null)
                throw new ArgumentException("Input of method CompareTo in " + this.GetType().Name + " is null");
            else if (other is RepairerLocationKnowledge)
                return -1;
            else
                throw new ArgumentException("Object : " + other.GetType().Name + " of CompareTo is not of type RepairerLocationKnowledge");
        }

        public string GetTypeToString()
        {
            return "repairerLocationKnowledge";
        }
    }
}
