using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using NabfProject.AI;
using NabfProject.KnowledgeManagerModel;

namespace NabfProject.NoticeBoardModel
{
    public abstract class Notice : IEquatable<Notice>, IComparable
    {
        public List<NodeKnowledge> WhichNodes { get; protected set; }
        public int AgentsNeeded { get; protected set; }
        public Int64 Id { get; private set; }
        public int Value { get; protected set; }
        public NoticeBoard.Status Status = NoticeBoard.Status.available;

        private int _highestAverageDesirabilityForNotice = -1;
        public int HighestAverageDesirabilityForNotice { get { return _highestAverageDesirabilityForNotice; } set { _highestAverageDesirabilityForNotice = value; } }
        private List<NabfAgent> _agentsApplied = new List<NabfAgent>();
        private List<NabfAgent> _topDesireAgents = new List<NabfAgent>();
        private Dictionary<NabfAgent, int> _agentsToDesirability = new Dictionary<NabfAgent, int>();

		public override string ToString()
		{
			string nodes = this.WhichNodes.Select(nk => nk.ToString() + ", ").Aggregate((i, j) => i + j);

			return "Notice: "+ this.GetType().Name +"( "+Id +" ), nodes: " + nodes;
		}

        public Notice(Int64 id)
        {
            Id = id;
        }

        public List<NabfAgent> GetTopDesireAgents()
        {
            return this._topDesireAgents.ToList();
        }

        public void AddToTopDesireAgents(NabfAgent toAdd)
        {
			//if (! _topDesireAgents.Exists (a => a.Id == toAdd.Id))
			_topDesireAgents.Add(toAdd);
        }

        public void AddRangeToTopDesireAgents(ICollection<NabfAgent> toAdd)
        {
			foreach (NabfAgent a in toAdd)
				AddToTopDesireAgents (a);
        }

        public List<NabfAgent> GetAgentsApplied()
        {
            return _agentsApplied.ToList();
        }

        public bool TryGetValueAgentToDesirabilityMap(NabfAgent agent, out int desire)
        {
            return _agentsToDesirability.TryGetValue(agent, out desire);
        }

        public bool ContentIsEqualTo(Notice no)
        {
            if (no == null)
                throw new ArgumentException("Input of ContentIsEqualTo of " + this.GetType().Name + " is null");
            else if (!(no is Notice))
                throw new ArgumentException("Object : " + no.GetType().Name + " of ContentIsEqualTo is not of type Notice");

			//return this.Id == no.Id;

            if (no.GetType() != this.GetType())
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

            return no.AgentsNeeded == this.AgentsNeeded && no.Value == this.Value ;
        }

        public void Apply(int desirability, NabfAgent a)
        {
			if (!_agentsApplied.Contains(a))
			{
				_agentsToDesirability.Add(a, desirability);
				_agentsApplied.Add(a);
			}
        }
        public void UnApply(NabfAgent a, NoticeBoard nb)
        {
            int des = -2;
            _agentsToDesirability.TryGetValue(a, out des);
            _agentsToDesirability.Remove(a);
            _agentsApplied.Remove(a);
            bool b = _topDesireAgents.Remove(a);
            bool success;
			int avg;
            List<NabfAgent> agentsToAdd;
            if (b)
            {
                success = nb.TryFindTopDesiresForNotice(this, out avg, out agentsToAdd);
                if (success)
                {
                    b = nb.RemoveJob(this);
                    if (b)
                    {
                        _topDesireAgents.Clear();
                        _topDesireAgents.AddRange(agentsToAdd);
						HighestAverageDesirabilityForNotice = avg;
                        nb.AddJob(this);
                    }
                }
                else
                    nb.RemoveJob(this);
            }
        }

        public void UpdateNotice(List<NodeKnowledge> whichNodes, List<NodeKnowledge> zoneNodes, int agentsNeeded, int value, string agentToRepair)
        {
            WhichNodes = whichNodes;
            AgentsNeeded = agentsNeeded;
            Value = value;

            if (this is RepairJob)
                ((RepairJob)this).AgentToRepair = agentToRepair;
            else if (this is OccupyJob)
                    ((OccupyJob)this).ZoneNodes = zoneNodes;
        }

        bool IEquatable<Notice>.Equals(Notice other)
        {
            if (other == null)
                return false;
            if (!(other is Notice))
                return false;
            return Id == other.Id;
        }

        int IComparable.CompareTo(object obj)
        {
            if (obj == null)
                throw new ArgumentException("Input of CompareTo of " + this.GetType().Name + " is null");
            else if (obj is Notice)
                if (((Notice)obj).HighestAverageDesirabilityForNotice < HighestAverageDesirabilityForNotice)
                    return -1;
                else if (((Notice)obj).HighestAverageDesirabilityForNotice > HighestAverageDesirabilityForNotice)
                    return 1;
                else
                    return 0;
            else
                throw new ArgumentException("Object : " + obj.GetType().Name + " of CompareTo is not of type Notice");
        }

        public bool IsEmpty()
        {
            return this is EmptyJob;
        }

		public virtual bool ContentIsSubsetOf(Notice n)
		{
			return this.WhichNodes.Intersect(n.WhichNodes).Count() > 0;
		}
	}
    
    public class DisruptJob : Notice
    {

        public DisruptJob(int agentsNeeded, List<NodeKnowledge> whichNodes, int value, Int64 id)
            : base(id)
        {
            AgentsNeeded = agentsNeeded;
            WhichNodes = whichNodes;
            Value = value;
        }

		
    }

    public class AttackJob : Notice
    {

        public AttackJob(int agentsNeeded, List<NodeKnowledge> whichNodes, int value, Int64 id)
            : base(id)
        {
            AgentsNeeded = agentsNeeded;
            WhichNodes = whichNodes;
            Value = value;
        }
		
    }

    public class OccupyJob : Notice
    {
        public List<NodeKnowledge> ZoneNodes { get; set; }

        public OccupyJob(int agentsNeeded, List<NodeKnowledge> whichNodes, List<NodeKnowledge> zoneNodes, int value, Int64 id)
            : base(id)
        {
            AgentsNeeded = agentsNeeded;
            WhichNodes = whichNodes;
            ZoneNodes = zoneNodes;
            Value = value;
        }

		public override bool ContentIsSubsetOf(Notice n)
		{
			if (n is OccupyJob)
			{
				var on = ((OccupyJob)n);
				return this.ZoneNodes.Intersect(on.ZoneNodes).Count() > 0;
			}
			return false;
		}
    }

    public class RepairJob : Notice
    {
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

    public class EmptyJob : Notice
    {
        public EmptyJob()
            : base(-1)
        {
            WhichNodes = new List<NodeKnowledge>();
            AgentsNeeded = 0;
            Value = 0;
        }
    }


}
