using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace NabfProject.KnowledgeManagerModel
{
    public class RoleKnowledge : Knowledge
    {
        public string Role { get; private set; }
        public string AgentId { get; private set; }
        public int Sureness { get; private set;}

        public RoleKnowledge(string role, string agentId, int sureness)
        {
            Role = role;
            AgentId = agentId;
            Sureness = sureness;
        }

        bool IEquatable<Knowledge>.Equals(Knowledge other)
        {
            if (other == null)
                throw new ArgumentException("Input of Equals of " + this.GetType().Name + " is null");
            else if (!(other is Knowledge))
                throw new ArgumentException("Object : " + other.GetType().Name + " of Equals is not implementing interface Knowledge");

            if (other.GetType() != this.GetType())
                return false;

            RoleKnowledge rk = (RoleKnowledge)other;

            return rk.AgentId == this.AgentId;
        }

        int IComparable<Knowledge>.CompareTo(Knowledge other)
        {
            if (other == null)
                throw new ArgumentException("Input of CompareTo of " + this.GetType().Name + " is null");
            else if (other is RoleKnowledge)
                if (((RoleKnowledge)other).Sureness < Sureness)
                    return -1;
                else if (((RoleKnowledge)other).Sureness > Sureness)
                    return 1;
                else
                    return 0;
            else
                throw new ArgumentException("Object : " + other.GetType().Name + " of CompareTo is not of type EdgeKnowledge");
        }

        public override bool Equals(object obj)
        {
            if (obj == null)
                throw new ArgumentException("Input of Equals of " + this.GetType().Name + " is null");
            else if (!(obj is Knowledge))
                throw new ArgumentException("Object : " + obj.GetType().Name + " of Equals is not implementing interface Knowledge");

            if (obj.GetType() != this.GetType())
                return false;

            RoleKnowledge rk = (RoleKnowledge)obj;

            return rk.AgentId == this.AgentId;
        }

        public string GetTypeToString()
        {
            return "roleKnowledge";
        }
    }
}
