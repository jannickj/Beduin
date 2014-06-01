using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.Data;
using JSLibrary.IiLang;
using JSLibrary.IiLang.Parameters;
using NabfProject;
using NabfProject.AI;
using NabfProject.Events;

namespace NabfProject.KnowledgeManagerModel
{
    public class KnowledgeManager
    {
        private HashSet<NabfAgent> _sharingList = new HashSet<NabfAgent>();

        private Dictionary<Knowledge,Knowledge> _knowledgeBase = new Dictionary<Knowledge,Knowledge>();

		public Knowledge[] KnowledgeBase
		{
			get { return _knowledgeBase.Keys.ToArray(); }
		} 

        
        public bool Subscribe(NabfAgent agent)
        {
            if (_sharingList.Contains(agent))
                return false;
            _sharingList.Add(agent);
            return true;
        }
        public bool Unsubscribe(NabfAgent agent)
        {
            return _sharingList.Remove(agent);
        }

        public void SendKnowledgeToManager(List<Knowledge> sentKnowledge, NabfAgent sender)
        {
            foreach (Knowledge k in sentKnowledge)
            {
				bool updatedKnowledge = false;
				if (!_knowledgeBase.ContainsKey(k))
				{
					_knowledgeBase.Add(k,k);
					updatedKnowledge = true;
				}
				else
				{
					var oldKnowledge = _knowledgeBase[k];
					if (oldKnowledge.CompareTo(k) > 0)
					{
						_knowledgeBase.Remove(k);
						_knowledgeBase.Add(k, k);
                        updatedKnowledge = true;
					}
				}

				if(updatedKnowledge)
					foreach (NabfAgent a in _sharingList)
					{
						if (a == sender)
							continue;
						a.Raise(new NewKnowledgeEvent(k));
					}
                
            }
                
        }
        
        public void SendOutAllKnowledgeToAgent(NabfAgent agent)
        {
            foreach (Knowledge k in _knowledgeBase.Keys)
            {
                agent.Raise(new NewKnowledgeEvent(k));
            }
        }


        
    }
}
