using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using NUnit.Framework;
using NabfProject.AI;
using System.Reflection;
using JSLibrary.Data;
using NabfProject.KnowledgeManagerModel;
using NabfProject.Events;

namespace NabfTest
{
    [TestFixture]
    public class KnowledgeManagerTest
    {
        KnowledgeManager km;
        NabfAgent agent1, agent2, agent3, agent4;
        NodeKnowledge nk1, nk2;
        EdgeKnowledge ek;
        public int evtTriggered;
        public List<Knowledge> k;

        [SetUp]
        public void Initialization()
        {
            km = new KnowledgeManager();
            agent1 = new NabfAgent("agent1");
            agent2 = new NabfAgent("agent2");
            agent3 = new NabfAgent("agent3");
            agent4 = new NabfAgent("agent4");
            nk1 = new NodeKnowledge("vertex1", 0);
            nk2 = new NodeKnowledge("vertex2", 0);
            ek = new EdgeKnowledge("vertex1", "vertex2", 0);

            evtTriggered = 0;
            k = new List<Knowledge>();
        }


        [Test]
        public void SubscribeAgent_NoDuplicate_Success()
        {
            km.Subscribe(agent1);
            object f = getField(km, false, "_sharingList");
            HashSet<NabfAgent> sharingList = (HashSet<NabfAgent>)f;
            Assert.AreEqual(1, sharingList.Count);

            km.Subscribe(agent2);
            f = getField(km, false, "_sharingList");
            sharingList = (HashSet<NabfAgent>)f;
            Assert.AreEqual(2, sharingList.Count);
        }

        [Test]
        public void SubscribeAgent_Duplicate_Failure()
        {
            km.Subscribe(agent1);
            km.Subscribe(agent2);
            km.Subscribe(agent1);
            object f = getField(km, false, "_sharingList");
            HashSet<NabfAgent> sharingList = (HashSet<NabfAgent>)f;
            Assert.AreEqual(2, sharingList.Count);
        }

        [Test]
        public void UnsubscribeAgent_Exists_Success()
        {
            km.Subscribe(agent1);
            km.Subscribe(agent2);
            object f = getField(km, false, "_sharingList");
            HashSet<NabfAgent> sharingList = (HashSet<NabfAgent>)f;
            Assert.AreEqual(2, sharingList.Count);

            km.Unsubscribe(agent1);
            f = getField(km, false, "_sharingList");
            sharingList = (HashSet<NabfAgent>)f;
            Assert.AreEqual(1, sharingList.Count);
        }

        [Test]
        public void UnsubscribeAgent_DontExists_Failure()
        {
            km.Subscribe(agent1);
            km.Subscribe(agent2);
            object f = getField(km, false, "_sharingList");
            HashSet<NabfAgent> sharingList = (HashSet<NabfAgent>)f;
            Assert.AreEqual(2, sharingList.Count);

            bool b = km.Unsubscribe(agent3);
            f = getField(km, false, "_sharingList");
            sharingList = (HashSet<NabfAgent>)f;
            Assert.AreEqual(2, sharingList.Count);
            Assert.IsFalse(b);
        }

        [Test]
        public void SendKnowledgeMixed_DontExists_Success()
        {
            km.Subscribe(agent1);
            km.Subscribe(agent2);
            km.Subscribe(agent3);

            agent1.Register(new XmasEngineModel.Management.Trigger<NewKnowledgeEvent>(CatchEvent));
            agent2.Register(new XmasEngineModel.Management.Trigger<NewKnowledgeEvent>(CatchEvent));
            agent3.Register(new XmasEngineModel.Management.Trigger<NewKnowledgeEvent>(CatchEvent));

            km.SendKnowledgeToManager(new List<Knowledge>() { nk1, ek }, agent1);
            km.SendKnowledgeToManager(new List<Knowledge>() { nk2 }, agent2);

            
            Assert.AreEqual(6, evtTriggered);
            Assert.IsTrue(k[0].Equals(nk1));
            Assert.IsTrue(k[1].Equals(nk1));
            Assert.IsTrue(k[2].Equals(ek));
            Assert.IsTrue(k[3].Equals(ek));

            Assert.IsTrue(k[4].Equals(nk2));
            Assert.IsTrue(k[5].Equals(nk2));
        }

        [Test]
        public void SendKnowledgeMixed_Exists_Failure()
        {
            km.Subscribe(agent1);
            km.Subscribe(agent2);
            km.Subscribe(agent3);

            agent1.Register(new XmasEngineModel.Management.Trigger<NewKnowledgeEvent>(CatchEvent));
            agent2.Register(new XmasEngineModel.Management.Trigger<NewKnowledgeEvent>(CatchEvent));
            agent3.Register(new XmasEngineModel.Management.Trigger<NewKnowledgeEvent>(CatchEvent));

            km.SendKnowledgeToManager(new List<Knowledge>() { nk1, ek }, agent1);
            km.SendKnowledgeToManager(new List<Knowledge>() { ek, nk1 }, agent2);

            
            Assert.AreEqual(4, evtTriggered);
            Assert.IsTrue(k[0].Equals(nk1));
            Assert.IsTrue(k[1].Equals(nk1));
            Assert.IsTrue(k[2].Equals(ek));
            Assert.IsTrue(k[3].Equals(ek));
        }

		[Test]
		public void AddKnowledge_BetterNodeAdded_RemoveTheOldKnowledge()
		{
			km.Subscribe(agent1);
			km.Subscribe(agent2);
		


			Knowledge nk1 = new NodeKnowledge("v1", 0);
			NodeKnowledge nk1better = new NodeKnowledge("v1", 7);

			km.SendKnowledgeToManager(new List<Knowledge>() { nk1, ek }, agent1);
			km.SendKnowledgeToManager(new List<Knowledge>() { ek, nk1better }, agent2);

			var knowledgeChosen = (NodeKnowledge) km.KnowledgeBase[0];

			Assert.AreEqual(nk1better.Value, knowledgeChosen.Value);
		}

		[Test]
		public void AddKnowledge_BetterEdgeAdded_RemoveTheOldKnowledge()
		{
			km.Subscribe(agent1);
			km.Subscribe(agent2);



			EdgeKnowledge ek1 = new EdgeKnowledge("v1", "v2", 0);
			EdgeKnowledge ek1better = new EdgeKnowledge("v2", "v1", 7);

			km.SendKnowledgeToManager(new List<Knowledge>() { ek1 }, agent1);
			km.SendKnowledgeToManager(new List<Knowledge>() { ek1better }, agent2);

			var knowledgeChosen = (EdgeKnowledge)km.KnowledgeBase[0];

			Assert.AreEqual(ek1better.Weight, knowledgeChosen.Weight);
		}

        private void CatchEvent(NewKnowledgeEvent evt)
        {
            k.Add(evt.NewKnowledge);
            evtTriggered++;
        }


        private object getField(object instance, bool useBase, String name)
        {
            Type t;
            if (useBase)
                t = instance.GetType().BaseType;
            else
                t = instance.GetType();

            FieldInfo f = t.GetField(name, BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.IgnoreCase);

            return f.GetValue(instance);
        }
    }
}
