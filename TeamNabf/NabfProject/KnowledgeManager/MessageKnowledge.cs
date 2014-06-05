using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace NabfProject.KnowledgeManagerModel
{
    public class MessageKnowledge : Knowledge
    {
        public string TargetedAgent { get; private set; }
        public string Message { get; private set; }

        public MessageKnowledge(string repairee, string message)
        {
            TargetedAgent = repairee;
            Message = message;
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
            return this.TargetedAgent.GetHashCode() + this.Message.GetHashCode();
		}

		public bool Equals(Knowledge other)
        {
            if (other == null)
                throw new ArgumentException("Input of method Equals in " + this.GetType().Name + " is null");
            else if (!(other is Knowledge))
                throw new ArgumentException("Object : " + other.GetType().Name + " of Equals is not implementing interface Knowledge");

            if (other.GetType() != this.GetType())
                return false;

            MessageKnowledge rlk = (MessageKnowledge)other;

			return (rlk.Message == this.Message && rlk.TargetedAgent == this.TargetedAgent);
        }

        int IComparable<Knowledge>.CompareTo(Knowledge other)
        {
            if (other == null)
                throw new ArgumentException("Input of method CompareTo in " + this.GetType().Name + " is null");
            else if (other is MessageKnowledge)
                return 1;
            else
                throw new ArgumentException("Object : " + other.GetType().Name + " of CompareTo is not of type MessageKnowledge");
        }

        public string GetTypeToString()
        {
            return "messageKnowledge";
        }
    }
}
