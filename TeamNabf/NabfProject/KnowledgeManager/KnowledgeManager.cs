﻿using System;
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
        
        private int _redudantEdgeKnowledgeCounter = 0;
        private int _redudantNodeKnowledgeCounter = 0;
        private int _redudantRoleKnowledgeCounter = 0;
        private int _redudantMessageKnowledgeCounter = 0;
        private int _redudantHeuristicKnowledgeCounter = 0;
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
            foreach (Knowledge k in sentKnowledge)
            {
				bool updatedKnowledge = false;
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
					var oldKnowledge = _knowledgeBase[k];
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
                            if (k is HeuristicKnowledge)
                            {
                                _redudantHeuristicKnowledgeCounter++;
                                if (_redudantHeuristicKnowledgeCounter % 100 == 0)
                                    Console.WriteLine("total numbers of redudant HEURISTIC knowledge is: " + _redudantHeuristicKnowledgeCounter);
                            }
                            if (k is MessageKnowledge)
                            {
                                _redudantMessageKnowledgeCounter++;
                                if (_redudantMessageKnowledgeCounter % 100 == 0)
                                    Console.WriteLine("total numbers of redudant MESSAGE knowledge is: " + _redudantMessageKnowledgeCounter);
                            }
                            if (k is EdgeKnowledge)
                            {
                                _redudantEdgeKnowledgeCounter++;
                                if (_redudantEdgeKnowledgeCounter % 100 == 0)
                                    Console.WriteLine("total numbers of redudant EDGE knowledge is: " + _redudantEdgeKnowledgeCounter);
                            }
                            if (k is NodeKnowledge)
                            {
                                _redudantNodeKnowledgeCounter++;
                                if (_redudantNodeKnowledgeCounter % 100 == 0)
                                    Console.WriteLine("total numbers of redudant NODE knowledge is: " + _redudantNodeKnowledgeCounter);
                            }
                            if (k is RoleKnowledge)
                            {
                                _redudantRoleKnowledgeCounter++;
                                if (_redudantRoleKnowledgeCounter % 100 == 0)
                                    Console.WriteLine("total numbers of redudant ROLE knowledge is: " + _redudantRoleKnowledgeCounter);
                            }

                        }
                    }
				}

                if (updatedKnowledge)
                {
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
