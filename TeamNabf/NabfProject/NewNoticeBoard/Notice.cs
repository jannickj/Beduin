using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using NabfProject.AI;
using NabfProject.KnowledgeManagerModel;

namespace NabfProject.NewNoticeBoardModel
{
    public abstract class NewNotice : IEquatable<NewNotice>, IComparable, IEqualityComparer<NewNotice>
    {
        public List<NodeKnowledge> WhichNodes { get; protected set; }
        public int AgentsNeeded { get; protected set; }
        public Int64 Id { get; private set; }
        public int Value { get; protected set; }
        public NewNoticeBoard.Status Status = NewNoticeBoard.Status.available;

        private int _avgDesirabilityAmongTopDesires = -1;
        public int AvgDesirabilityAmongTopDesires { get { return _avgDesirabilityAmongTopDesires; } set { _avgDesirabilityAmongTopDesires = value; } }

        private List<NabfAgent> _agentsApplied = new List<NabfAgent>();
        private List<NabfAgent> _agentsOnJob = new List<NabfAgent>();
        private Dictionary<string, int> _agentNameToDesirability = new Dictionary<string, int>();
        public abstract NewNoticeBoard.JobType GetNoticeType();


        public NewNotice(Int64 id)
        {
            Id = id;
        }


        public void AddToAgentsOnJob(NabfAgent toAdd)
        {
            _agentsOnJob.Add(toAdd);
        }
        public void AddRangeToAgentsOnJob(ICollection<NabfAgent> toAdd)
        {
            foreach (NabfAgent a in toAdd)
                AddToAgentsOnJob(a);
        }
        public List<NabfAgent> GetAgentsOnJob()
        {
            return this._agentsOnJob.ToList();
        }
        public List<NabfAgent> GetAgentsApplied()
        {
            return _agentsApplied.ToList();
        }
        public void ClearAgentsOnJob()
        {
            _agentsOnJob.Clear();
        }

        public bool TryGetDesirabilityOfAgent(NabfAgent agent, out int desire)
        {
            return _agentNameToDesirability.TryGetValue(agent.Name, out desire);
        }


        //not impl yet
        public void Apply(int desirability, NabfAgent a)
        {
            throw new NotImplementedException();
        }
        //not impl yet
        public void UnApply(NabfAgent a)
        {
            throw new NotImplementedException();
        }
        //not impl yet
        public void UpdateNotice(List<NodeKnowledge> whichNodes, int agentsNeeded, int value, List<NodeKnowledge> zoneNodes, string agentToRepair)
        {
            WhichNodes = whichNodes;
            AgentsNeeded = agentsNeeded;
            Value = value;

            if (this is RepairJob)
                ((RepairJob)this).AgentToRepair = agentToRepair;
            else if (this is OccupyJob)
                ((OccupyJob)this).ZoneNodes = zoneNodes;
        }


		public virtual bool ContentIsSubsetOf(NewNotice n)
		{
            if (n.GetNoticeType() != this.GetNoticeType())
                return false;
			return this.WhichNodes.Intersect(n.WhichNodes).Count() > 0;
		}
        public bool ContentIsEqualTo(NewNotice no)
        {
            if (no == null)
                throw new ArgumentException("Input of ContentIsEqualTo of " + this.GetType().Name + " is null");
            else if (!(no is NewNotice))
                throw new ArgumentException("Object : " + no.GetType().Name + " of ContentIsEqualTo is not of type Notice");

            if (no.GetNoticeType() != this.GetNoticeType())
                return false;
            else if (this is RepairJob)
            {
                if (((RepairJob)this).AgentToRepair != ((RepairJob)no).AgentToRepair)
                    return false;
            }
            else if (this is OccupyJob)
                if (((OccupyJob)no).ZoneNodes.Except<NodeKnowledge>(((OccupyJob)this).ZoneNodes).Count() != 0)
                    return false;

            if (no.WhichNodes.Except<NodeKnowledge>(this.WhichNodes).Count() != 0)
                return false;

            return no.AgentsNeeded == this.AgentsNeeded && no.Value == this.Value;
        }

        int IComparable.CompareTo(object obj)
        {
            if (obj == null)
                throw new ArgumentException("Input of CompareTo of " + this.GetType().Name + " is null");
            else if (obj is NewNotice)
                if (((NewNotice)obj).AvgDesirabilityAmongTopDesires < AvgDesirabilityAmongTopDesires)
                    return -1;
                else if (((NewNotice)obj).AvgDesirabilityAmongTopDesires > AvgDesirabilityAmongTopDesires)
                    return 1;
                else
                    return 0;
            else
                throw new ArgumentException("Object : " + obj.GetType().Name + " of CompareTo is not of type Notice");
        }

        public override bool Equals(object obj)
        {
            if (obj == null)
                return false;
            if (!(obj is NewNotice))
                return false;
            return Id == ((NewNotice)obj).Id;
        }
        bool IEquatable<NewNotice>.Equals(NewNotice other)
        {
            if (other == null)
                return false;
            if (!(other is NewNotice))
                return false;
            return Id == other.Id;
        }
        bool IEqualityComparer<NewNotice>.Equals(NewNotice x, NewNotice y)
        {
            return x.Id == y.Id;
        }

        int IEqualityComparer<NewNotice>.GetHashCode(NewNotice obj)
        {
            return obj.Id.GetHashCode();
        }
        public override int GetHashCode()
        {
            return this.Id.GetHashCode();
        }

        public bool IsEmpty()
        {
            return this is EmptyJob;
        }

        public override string ToString()
        {
            string nodes = this.WhichNodes.Select(nk => nk.ToString() + ", ").Aggregate((i, j) => i + j);

            return "Notice: " + this.GetType().Name + "(" + Id + "), nodes: " + nodes;
        }
    }
    
    public class DisruptJob : NewNotice
    {
        public override NewNoticeBoard.JobType GetNoticeType()
        {
            return NewNoticeBoard.JobType.Disrupt;
        }

        public DisruptJob(int agentsNeeded, List<NodeKnowledge> whichNodes, int value, Int64 id)
            : base(id)
        {
            AgentsNeeded = agentsNeeded;
            WhichNodes = whichNodes;
            Value = value;
        }

		
    }

    public class AttackJob : NewNotice
    {
        public override NewNoticeBoard.JobType GetNoticeType()
        {
            return NewNoticeBoard.JobType.Attack;
        }

        public AttackJob(int agentsNeeded, List<NodeKnowledge> whichNodes, int value, Int64 id)
            : base(id)
        {
            AgentsNeeded = agentsNeeded;
            WhichNodes = whichNodes;
            Value = value;
        }
		
    }

    public class OccupyJob : NewNotice
    {
        public override NewNoticeBoard.JobType GetNoticeType()
        {
            return NewNoticeBoard.JobType.Occupy;
        }
        public List<NodeKnowledge> ZoneNodes { get; set; } //the nodes which is part of the zone (not the nodes to stand on)

        public OccupyJob(int agentsNeeded, List<NodeKnowledge> whichNodes, List<NodeKnowledge> zoneNodes, int value, Int64 id)
            : base(id)
        {
            AgentsNeeded = agentsNeeded;
            WhichNodes = whichNodes;
            ZoneNodes = zoneNodes;
            Value = value;
        }

		public override bool ContentIsSubsetOf(NewNotice n)
		{
			if (n is OccupyJob)
			{
				var on = ((OccupyJob)n);
				return this.ZoneNodes.Intersect(on.ZoneNodes).Count() > 0;
			}
			return false;
		}
    }

    public class RepairJob : NewNotice
    {
        public override NewNoticeBoard.JobType GetNoticeType()
        {
            return NewNoticeBoard.JobType.Repair;
        }
        public string AgentToRepair { get; set; }

        public RepairJob(List<NodeKnowledge> whichNodes, string agentToRepair, int value, Int64 id)
            : base(id)
        {
            WhichNodes = whichNodes;
            AgentsNeeded = 1;
            AgentToRepair = agentToRepair;
            Value = value;
        }

		
    }

    public class EmptyJob : NewNotice
    {
        public override NewNoticeBoard.JobType GetNoticeType()
        {
            return NewNoticeBoard.JobType.Empty;
        }
        public EmptyJob()
            : base(-1)
        {
            WhichNodes = new List<NodeKnowledge>();
            AgentsNeeded = 0;
            Value = 0;
        }
    }


}
