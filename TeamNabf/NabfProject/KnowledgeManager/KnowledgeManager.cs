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

        //private Dictionary<Knowledge, bool> _knowledgeBase = new Dictionary<Knowledge, bool>();
		private Dictionary<Knowledge,Knowledge> _knowledgeBase = new Dictionary<Knowledge,Knowledge>();
        
        public int _redudantEdgeKnowledgeCounter = 0;
        public int _redudantNodeKnowledgeCounter = 0;
        public int _redudantRoleKnowledgeCounter = 0;
        public int _redudantMessageKnowledgeCounter = 0;
        public int _redudantHeuristicKnowledgeCounter = 0;
        public int _nodeKnowledgeCounter = 0;
        public int _edgeKnowledgeCounter = 0;
        public int _roleKnowledgeCounter = 0;
        public int _heuristicKnowledgeCounter = 0;
        public int _messageKnowledgeCounter = 0;
        public int _callsToSendKnowledge = 0;
        private const bool verbose = true;

		public Knowledge[] KnowledgeBase
		{
			get { return _knowledgeBase.Keys.ToArray(); }
		} 

        //private DictionaryList<Knowledge, NabfAgent> _knowledgeToAgent = new DictionaryList<Knowledge, NabfAgent>();
        //private DictionaryList<NabfAgent, Knowledge> _agentToKnowledge = new DictionaryList<NabfAgent, Knowledge>();

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
            //Knowledge kl;
            Knowledge oldKnowledge;
            bool updatedKnowledge;
            foreach (Knowledge k in sentKnowledge)
            {
				updatedKnowledge = false;
				if (!_knowledgeBase.ContainsKey(k))
				{
                    if (!(k is MessageKnowledge))
					    _knowledgeBase.Add(k,k);
					updatedKnowledge = true;
					//_knowledgeToAgent.Add(k, sender);
					//_agentToKnowledge.Add(sender, k);
				}
				else
				{
                    oldKnowledge = _knowledgeBase[k];
                    if (oldKnowledge.CompareTo(k) > 0)
                    {
                        _knowledgeBase.Remove(k);
                        _knowledgeBase.Add(k, k);
                        updatedKnowledge = true;
                    }
                    else
                    {                        
                        if (verbose)
                        {
                            #region debug code
                            if (k is HeuristicKnowledge)
                            {
                                _redudantHeuristicKnowledgeCounter++;
                                if (_redudantHeuristicKnowledgeCounter % 1000 == 0)
                                    Console.WriteLine("total numbers of --REDUDANT-- HEURISTIC knowledge is: " + _redudantHeuristicKnowledgeCounter);
                            }
                            if (k is MessageKnowledge)
                            {
                                _redudantMessageKnowledgeCounter++;
                                if (_redudantMessageKnowledgeCounter % 200 == 0)
                                    Console.WriteLine("total numbers of --REDUDANT-- MESSAGE knowledge is: " + _redudantMessageKnowledgeCounter);
                            }
                            if (k is EdgeKnowledge)
                            {
                                _redudantEdgeKnowledgeCounter++;
                                if (_redudantEdgeKnowledgeCounter % 200 == 0)
                                    Console.WriteLine("total numbers of --REDUDANT-- EDGE knowledge is: " + _redudantEdgeKnowledgeCounter);
                            }
                            if (k is NodeKnowledge)
                            {
                                _redudantNodeKnowledgeCounter++;
                                if (_redudantNodeKnowledgeCounter % 200 == 0)
                                    Console.WriteLine("total numbers of --REDUDANT-- NODE knowledge is: " + _redudantNodeKnowledgeCounter);
                            }
                            if (k is RoleKnowledge)
                            {
                                _redudantRoleKnowledgeCounter++;
                                if (_redudantRoleKnowledgeCounter % 10 == 0)
                                    Console.WriteLine("total numbers of --REDUDANT-- ROLE knowledge is: " + _redudantRoleKnowledgeCounter);
                            }
                            #endregion
                        }
                    }
				}

                if (updatedKnowledge)
                {
                    if (verbose)
                    {
                        #region debug code
                        if (k is HeuristicKnowledge)
                        {
                            _heuristicKnowledgeCounter++;
                            if (_heuristicKnowledgeCounter % 1000 == 0)
                                Console.WriteLine("total numbers of sent HEURISTIC knowledge is: " + _heuristicKnowledgeCounter);
                        }
                        if (k is MessageKnowledge)
                        {
                            _messageKnowledgeCounter++;
                            if (_messageKnowledgeCounter % 100 == 0)
                                Console.WriteLine("total numbers of sent MESSAGE knowledge is: " + _messageKnowledgeCounter);
                        }
                        if (k is EdgeKnowledge)
                        {
                            _edgeKnowledgeCounter++;
                            if (_edgeKnowledgeCounter % 50 == 0)
                                Console.WriteLine("total numbers of sent EDGE knowledge is: " + _edgeKnowledgeCounter);
                        }
                        if (k is NodeKnowledge)
                        {
                            _nodeKnowledgeCounter++;
                            if (_nodeKnowledgeCounter % 50 == 0)
                                Console.WriteLine("total numbers of sent NODE knowledge is: " + _nodeKnowledgeCounter);
                        }
                        if (k is RoleKnowledge)
                        {
                            _roleKnowledgeCounter++;
                            if (_roleKnowledgeCounter % 2 == 0)
                                Console.WriteLine("total numbers of sent ROLE knowledge is: " + _roleKnowledgeCounter);
                        }
                        #endregion
                    }
                    foreach (NabfAgent a in _sharingList)
                    {
                        if (a == sender)
                            continue;
                        if (k is MessageKnowledge)
                            if (((MessageKnowledge)k).TargetedAgent == a.Name)
                                a.Raise(new NewKnowledgeEvent(k));
                            else
                                continue;
                        a.Raise(new NewKnowledgeEvent(k));
                    }
                }
                //else
                //{
                    //kl = _knowledgeBase.Keys.First(pk => k.Equals(pk));
                    //_knowledgeToAgent.Add(kl, sender);
                    //_agentToKnowledge.Add(sender, kl);
                //}
            }
            //SendKnowledgeToSubscribedAgents();   
            _callsToSendKnowledge++;
            if (_callsToSendKnowledge % 50 == 0)
                Console.WriteLine("--------size of Knowledge Base is: " + _knowledgeBase.Keys.Count);
        }
        
        public void SendOutAllKnowledgeToAgent(NabfAgent agent)
        {
            foreach (Knowledge k in _knowledgeBase.Keys)
            {
                if (k is MessageKnowledge)
                    if (((MessageKnowledge)k).TargetedAgent == agent.Name)
                        agent.Raise(new NewKnowledgeEvent(k));
                    else
                        continue;
                agent.Raise(new NewKnowledgeEvent(k));
            }
        }


        //private void SendKnowledgeToSubscribedAgents()
        //{
        //    foreach(KeyValuePair<Knowledge, bool> kvp in _knowledgeBase)
        //    {
        //        if (kvp.Value)
        //            continue;


        //    }
        //}

        
    }
}
