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


        private const bool verbose = false;
        //status reporting for SimMan
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

        public int GetSubscribedAgentsCount()
        {
            return _sharingList.Count;
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
                        #region debug code
                        if (k is HeuristicKnowledge)
                        {
                            _redudantHeuristicKnowledgeCounter++;
                            if (verbose && _redudantHeuristicKnowledgeCounter % 1000 == 0)
                                Console.WriteLine("Total number of --REDUDANT-- HEURISTIC knowledge is: " + _redudantHeuristicKnowledgeCounter);
                        }
                        else if (k is MessageKnowledge)
                        {
                            _redudantMessageKnowledgeCounter++;
                            if (verbose && _redudantMessageKnowledgeCounter % 200 == 0)
                                Console.WriteLine("Total number of --REDUDANT-- MESSAGE knowledge is: " + _redudantMessageKnowledgeCounter);
                        }
                        else if (k is EdgeKnowledge)
                        {
                            _redudantEdgeKnowledgeCounter++;
                            if (verbose && _redudantEdgeKnowledgeCounter % 200 == 0)
                                Console.WriteLine("Total number of --REDUDANT-- EDGE knowledge is: " + _redudantEdgeKnowledgeCounter);
                        }
                        else if (k is NodeKnowledge)
                        {
                            _redudantNodeKnowledgeCounter++;
                            if (verbose && _redudantNodeKnowledgeCounter % 200 == 0)
                                Console.WriteLine("Total number of --REDUDANT-- NODE knowledge is: " + _redudantNodeKnowledgeCounter);
                        }
                        else if (k is RoleKnowledge)
                        {
                            _redudantRoleKnowledgeCounter++;
                            if (verbose && _redudantRoleKnowledgeCounter % 10 == 0)
                                Console.WriteLine("Total number of --REDUDANT-- ROLE knowledge is: " + _redudantRoleKnowledgeCounter);
                        }
                        #endregion
                    }
				}

                if (updatedKnowledge)
                {
                    #region debug code
                    if (k is HeuristicKnowledge)
                    {
                        _heuristicKnowledgeCounter++;
                        if (verbose && _heuristicKnowledgeCounter % 1000 == 0)
                            Console.WriteLine("Total number of sent HEURISTIC knowledge is: " + _heuristicKnowledgeCounter);
                    }
                    if (k is MessageKnowledge)
                    {
                        _messageKnowledgeCounter++;
                        if (verbose && _messageKnowledgeCounter % 100 == 0)
                            Console.WriteLine("Total number of sent MESSAGE knowledge is: " + _messageKnowledgeCounter);
                    }
                    if (k is EdgeKnowledge)
                    {
                        _edgeKnowledgeCounter++;
                        if (verbose && _edgeKnowledgeCounter % 50 == 0)
                            Console.WriteLine("Total number of sent EDGE knowledge is: " + _edgeKnowledgeCounter);
                    }
                    if (k is NodeKnowledge)
                    {
                        _nodeKnowledgeCounter++;
                        if (verbose && _nodeKnowledgeCounter % 50 == 0)
                            Console.WriteLine("Total number of sent NODE knowledge is: " + _nodeKnowledgeCounter);
                    }
                    if (k is RoleKnowledge)
                    {
                        _roleKnowledgeCounter++;
                        if (verbose && _roleKnowledgeCounter % 2 == 0)
                            Console.WriteLine("Total number of sent ROLE knowledge is: " + _roleKnowledgeCounter);
                    }
                    #endregion
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
            if (verbose && _callsToSendKnowledge % 50 == 0)
                Console.WriteLine("Size of Knowledge Base is: " + _knowledgeBase.Keys.Count);
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
