using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using NUnit.Framework;
using NabfProject.NoticeBoardModel;
using NabfProject.AI;
using System.Reflection;
using JSLibrary.Data;
using NabfProject.KnowledgeManagerModel;
using NabfProject.Events;

namespace NabfTest
{
    //[TestFixture]
    //public class NoticeBoardTest
    //{
    //    NoticeBoard nb;
    //    int ID = 0;
    //    int evtTriggered;
    //    int maxDesirabilityForFirstNotice = -1, maxDesirabilityForSecondNotice = -1, maxDesirabilityForThirdNotice = -1;
    //    int agentsAppliedToFirstNotice = -1, agentsAppliedToSecondNotice = -1, agentsAppliedToThirdNotice = -1;
    //    NabfAgent agentAppliedToFirstNotice1 = null, agentAppliedToFirstNotice2 = null, agentAppliedToSecondNotice1 = null, agentAppliedToSecondNotice2 = null, agentAppliedToThirdNotice = null;
    //    NabfAgent agentOnFirstNotice1 = null, agentOnFirstNotice2 = null, agentOnSecondNotice = null, agentOnThirdNotice = null;
    //    int i = 0;
    //    Notice firstNotice = null, secondNotice = null, thirdNotice = null;
    //    List<Notice> notices = new List<Notice>();
    //    List<List<NabfAgent>> listOfListOfAgents = new List<List<NabfAgent>>();
    //    List<int> averageDesires = new List<int>();
    //    List<NodeKnowledge> emptyZone = new List<NodeKnowledge>();
    //    string emptyAgentName = "";


    //    [SetUp]
    //    public void Initialization()
    //    {
    //        nb = new NoticeBoard();
    //        evtTriggered = 0;
    //    }

    //    [Test]
    //    public void AddInitialNotice_NoDuplicateListEmpty_Success()
    //    {
    //        //Assert.Pass("test disabled");

    //        Notice n;
    //        bool addSuccess = nb.CreateAndAddNotice(NoticeBoard.JobType.Disrupt, 1, null, null, null, 0, out n);
    //        Assert.True(addSuccess);
    //        Assert.AreEqual(1, nb.GetNoticeCount());
    //        Assert.IsTrue(n.Equals(nb.GetNotices().First()));
    //    }

    //    [Test]
    //    public void AddNotice_DuplicateExists_Failure()
    //    {
    //        //Assert.Pass("test disabled");

    //        List<NodeKnowledge> testNodes = new List<NodeKnowledge>() { new NodeKnowledge("n1"), new NodeKnowledge("n2") };

    //        Notice no = new DisruptJob(2, testNodes, 0, ID++);
    //        Notice no2 = new DisruptJob(2, testNodes, 0, ID++);
    //        Notice no3 = new AttackJob(2, testNodes, 0, ID++);
    //        Notice no4 = new AttackJob(2, testNodes, 0, ID++);
    //        Notice no5 = new OccupyJob(2, testNodes, testNodes, 0, ID++);
    //        Notice no6 = new OccupyJob(2, testNodes, testNodes, 0, ID++);
    //        Notice no7 = new RepairJob(testNodes, "", 0, ID++);
    //        Notice no8 = new RepairJob(testNodes, "", 0, ID++);
    //        bool addSuccess = nb.CreateAndAddNotice(NoticeBoard.JobType.Disrupt, 2, testNodes, emptyZone, emptyAgentName, 0, out no);
    //        bool addSuccess2 = nb.CreateAndAddNotice(NoticeBoard.JobType.Disrupt, 2, testNodes, emptyZone, emptyAgentName, 0, out no2);
    //        bool addSuccess3 = nb.CreateAndAddNotice(NoticeBoard.JobType.Attack, 2, testNodes, emptyZone, emptyAgentName, 0, out no3);
    //        bool addSuccess4 = nb.CreateAndAddNotice(NoticeBoard.JobType.Attack, 2, testNodes, emptyZone, emptyAgentName, 0, out no4);
    //        bool addSuccess5 = nb.CreateAndAddNotice(NoticeBoard.JobType.Occupy, 2, testNodes, emptyZone, emptyAgentName, 0, out no5);
    //        bool addSuccess6 = nb.CreateAndAddNotice(NoticeBoard.JobType.Occupy, 2, testNodes, emptyZone, emptyAgentName, 0, out no6);
    //        bool addSuccess7 = nb.CreateAndAddNotice(NoticeBoard.JobType.Repair, 1, testNodes, emptyZone, emptyAgentName, 0, out no7);
    //        bool addSuccess8 = nb.CreateAndAddNotice(NoticeBoard.JobType.Repair, 1, testNodes, emptyZone, emptyAgentName, 0, out no8);
    //        Assert.True(addSuccess);
    //        Assert.False(addSuccess2);
    //        Assert.True(addSuccess3);
    //        Assert.False(addSuccess4);
    //        Assert.True(addSuccess5);
    //        Assert.False(addSuccess6);
    //        Assert.True(addSuccess7);
    //        Assert.False(addSuccess8);
    //        Assert.AreEqual(4, nb.GetNoticeCount());
    //    }

    //    [Test]
    //    public void ApplyToNotice_DuplicateDesirability_Success()
    //    {
    //        //Assert.Pass("test disabled");
            
    //        Notice n;
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Disrupt, 2
    //            , new List<NodeKnowledge>() { new NodeKnowledge("") }, new List<NodeKnowledge>() { new NodeKnowledge("") }, "", 99, out n);

    //        NabfAgent agent1 = new NabfAgent("Nabf1"), agent2 = new NabfAgent("2nabf"), agent3 = new NabfAgent("nabf30"), agent4 = new NabfAgent("nabf");

    //        //nb.ApplyToNotice(n, 1, agent1);
    //        nb.ApplyToNotice(n, 2, agent2);
    //        nb.ApplyToNotice(n, 2, agent3);


    //        nb.FindJobsForAgents();

    //        Assert.AreEqual(agent2.Name, n.GetTopDesireAgents()[0].Name);
    //        Assert.AreEqual(agent3.Name, n.GetTopDesireAgents()[1].Name);
    //    }

    //    //[Test]
    //    //public void meh_meh_meh()
    //    //{
    //    //    //Notice n;
    //    //    //nb.CreateAndAddNotice(NoticeBoard.JobType.Disrupt, 2
    //    //    //    , new List<NodeKnowledge>() { new NodeKnowledge("") }, new List<NodeKnowledge>() { new NodeKnowledge("") }, "", 99, out n);

    //    //    //NabfAgent agent1 = new NabfAgent("Nabf1"), agent2 = new NabfAgent("2nabf"), agent3 = new NabfAgent("nabf30"), agent4 = new NabfAgent("nabf");

    //    //    ////nb.ApplyToNotice(n, 1, agent1);
    //    //    //nb.ApplyToNotice(n, 2, agent2);
    //    //    //nb.ApplyToNotice(n, 2, agent3);

    //    //    NodeKnowledge nk1 = new NodeKnowledge("q");
    //    //    NodeKnowledge nk2 = new NodeKnowledge("q");

    //    //    List<NodeKnowledge> nks1 = new List<NodeKnowledge>() { nk1 };
    //    //    List<NodeKnowledge> nks2 = new List<NodeKnowledge>() { nk2 };

    //    //    var qq = nks1.Except(nks2);

    //    //    Assert.AreEqual(0, qq.Count());

    //    //}
        
    //    [Test]
    //    public void RemoveNotice_MultipleNotices_Success()
    //    {
    //        //Assert.Pass("test disabled");

    //        List<NodeKnowledge> testNodes = new List<NodeKnowledge>() { new NodeKnowledge("n1"), new NodeKnowledge("n2") };
    //        List<NodeKnowledge> testNodes2 = new List<NodeKnowledge>() { new NodeKnowledge("n1"), new NodeKnowledge("n2") };

    //        Notice no = new DisruptJob(2, testNodes, 0, ID++);
    //        Notice no2 = new DisruptJob(2, testNodes2, 0, ID++);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Disrupt, 2, testNodes, emptyZone, emptyAgentName, 0, out no);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Disrupt, 2, testNodes2, emptyZone, emptyAgentName, 0, out no2);
    //        bool removeSuccess = nb.RemoveNotice(no2);
    //        Assert.True(removeSuccess);
    //        Assert.AreEqual(1, nb.GetNoticeCount());
    //    }

    //    [Test]
    //    public void RemoveNotice_NoSuchNotice_Failure()
    //    {
    //        //Assert.Pass("test disabled");

    //        List<NodeKnowledge> testNodes = new List<NodeKnowledge>() { new NodeKnowledge("n1"), new NodeKnowledge("n2") };

    //        Notice no = new DisruptJob(1, testNodes, 0, ID++);
    //        Notice no2 = new DisruptJob(3, testNodes, 0, ID++);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Disrupt, 1, testNodes, emptyZone, emptyAgentName, 0, out no);
    //        bool removeSuccess = nb.RemoveNotice(no2);
    //        Assert.False(removeSuccess);
    //        Assert.AreEqual(1, nb.GetNoticeCount());
    //    }

    //    [Test]
    //    public void GetNoticeOfType_NoJobsOfSuchType_Failure()
    //    {
    //        //Assert.Pass("test disabled");

    //        List<NodeKnowledge> testNodes = new List<NodeKnowledge>() { new NodeKnowledge("n1"), new NodeKnowledge("n2") };

    //        Notice no = new DisruptJob(1, testNodes, 0, ID++);
    //        Notice no2 = new AttackJob(1, testNodes, 0, ID++);
    //        Notice no3 = new OccupyJob(1, testNodes, emptyZone, 0, ID++);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Disrupt, 1, testNodes, emptyZone, emptyAgentName, 0, out no);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Attack, 1, testNodes, emptyZone, emptyAgentName, 0, out no2);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Occupy, 1, testNodes, emptyZone, emptyAgentName, 0, out no3);
    //        List<NoticeBoard.JobType> jobs = new List<NoticeBoard.JobType>() { NoticeBoard.JobType.Repair };
    //        List<Notice> possibleJobs = new List<Notice>();
    //        possibleJobs.AddRange(nb.GetNotices(jobs));

    //        Assert.AreEqual(0, possibleJobs.Count);
    //    }

    //    [Test]
    //    public void GetNoticeOfType_MultipleJobsOfTypeAndOtherType_Success()
    //    {
    //        //Assert.Pass("test disabled");

    //        List<NodeKnowledge> testNodes = new List<NodeKnowledge>() { new NodeKnowledge("n1"), new NodeKnowledge("n2") };

    //        Notice no = new DisruptJob(1, testNodes, 0, ID++);
    //        Notice no2 = new AttackJob(1, testNodes, 0, ID++);
    //        Notice no3 = new OccupyJob(1, testNodes, emptyZone, 0, ID++);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Disrupt, 1, testNodes, emptyZone, emptyAgentName, 0, out no);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Attack, 1, testNodes, emptyZone, emptyAgentName, 0, out no2);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Occupy, 1, testNodes, emptyZone, emptyAgentName, 0, out no3);
    //        List<NoticeBoard.JobType> jobs = new List<NoticeBoard.JobType>(){ NoticeBoard.JobType.Attack, NoticeBoard.JobType.Occupy };
    //        List<Notice> possibleJobs = new List<Notice>();
    //        possibleJobs.AddRange(nb.GetNotices(jobs));

    //        Assert.AreEqual(2, possibleJobs.Count);
    //    }

    //    [Test]
    //    public void TwoAgentsApplyToSameNoticeThenUnApply_NoticeExists_Success()
    //    {
    //        //Assert.Pass("test disabled");

    //        List<NodeKnowledge> testNodes = new List<NodeKnowledge>() { new NodeKnowledge("n1"), new NodeKnowledge("n2") };

    //        Notice no = new DisruptJob(1, testNodes, 0, ID++);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Disrupt, 1, testNodes, emptyZone, emptyAgentName, 0, out no);
    //        List<NoticeBoard.JobType> jobs = new List<NoticeBoard.JobType>() { NoticeBoard.JobType.Disrupt };
    //        List<Notice> possibleJobs = new List<Notice>();
    //        possibleJobs.AddRange(nb.GetNotices(jobs));

    //        Assert.AreEqual(1, possibleJobs.Count);

    //        int desirability = 1, desirability2 = 99;
    //        NabfAgent a1 = new NabfAgent("agent1"), a2 = new NabfAgent("agent2");
    //        Notice n = nb.GetNotices(jobs).First<Notice>();


    //        nb.ApplyToNotice(possibleJobs[0], desirability, a1);
    //        Assert.AreEqual(1, n.GetAgentsApplied().Count);

    //        nb.ApplyToNotice(possibleJobs[0], desirability2, a2);
    //        Assert.AreEqual(2, n.GetAgentsApplied().Count);


    //        nb.UnApplyToNotice(possibleJobs[0], a2, false);
    //        Assert.AreEqual(1, n.GetAgentsApplied().Count);
    //    }

    //    [Test]
    //    public void UpdateNotice_NoticeExists_Success()
    //    {
    //        //Assert.Pass("test disabled");

    //        Notice no = new DisruptJob(1, null, 0, 0);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Disrupt, 1, null, null, null, 0, out no);
    //        Assert.AreEqual(1, nb.GetNotices(new List<NoticeBoard.JobType>(){ NoticeBoard.JobType.Disrupt}).First<Notice>().AgentsNeeded);
    //        bool updateSuccess = nb.UpdateNotice(no.Id, new List<NodeKnowledge>(), null, 10, 0, null);
    //        Assert.IsTrue(updateSuccess);
    //        Assert.AreEqual(10, nb.GetNotices(new List<NoticeBoard.JobType>() { NoticeBoard.JobType.Disrupt }).First<Notice>().AgentsNeeded);
    //    }

    //    [Test]
    //    public void UpdateNotice_NoticeDontExists_Failure()
    //    {
    //        //Assert.Pass("test disabled");

    //        bool updateSuccess = nb.UpdateNotice(0, new List<NodeKnowledge>(), null, 1, 0, null);
    //        Assert.IsFalse(updateSuccess);
    //    }

    //    [Test]
    //    public void FindJobsForAgents_AllJobsFilledAllTheTime_Success()
    //    {
    //        //Assert.Pass("test disabled");

    //        List<NodeKnowledge> testNodes = new List<NodeKnowledge>() { new NodeKnowledge("n1"), new NodeKnowledge("n2") };

    //        Notice no = new DisruptJob(1, testNodes, 0, ID++)
    //            , no2 = new AttackJob(1, testNodes, 0, ID++)
    //            , no3 = new OccupyJob(2, testNodes, emptyZone, 0, ID++);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Disrupt, 1, testNodes, emptyZone, emptyAgentName, 0, out no);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Attack, 1, testNodes, emptyZone, emptyAgentName, 0, out no2);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Occupy, 2, testNodes, emptyZone, emptyAgentName, 0, out no3);
            
    //        NabfAgent a1 = new NabfAgent("agent1")
    //            , a2 = new NabfAgent("agent2")
    //            , a3 = new NabfAgent("agent3")
    //            , a4 = new NabfAgent("agent4");

    //        //requires 1 agent
    //        nb.ApplyToNotice(no, 1, a1);
    //        nb.ApplyToNotice(no, 50, a2);
    //        nb.ApplyToNotice(no, 1, a3);

    //        //requires 1 agent
    //        nb.ApplyToNotice(no2, 50, a1);
    //        nb.ApplyToNotice(no2, 1, a2);

    //        //requires 2 agent
    //        nb.ApplyToNotice(no3, 50, a3);
    //        nb.ApplyToNotice(no3, 100, a4);

    //        int order = 1;
    //        bool failed = false;
    //        Action<int, Notice> checkOrder = (oId, n) => { if ((order == oId)) { IsNoticeCorrect1(oId, n); order++; } else failed = true; };

    //        a1.Register(new XmasEngineModel.Management.Trigger<ReceivedJobEvent>(evt => checkOrder(3, evt.Notice)));
    //        a2.Register(new XmasEngineModel.Management.Trigger<ReceivedJobEvent>(evt => checkOrder(2, evt.Notice)));
    //        a3.Register(new XmasEngineModel.Management.Trigger<ReceivedJobEvent>(evt => checkOrder(1, evt.Notice)));
    //        a4.Register(new XmasEngineModel.Management.Trigger<ReceivedJobEvent>(evt => checkOrder(0, evt.Notice)));

    //        nb.FindJobsForAgents();



    //        //Assert.AreEqual(3, evtTriggered);
    //        Assert.IsTrue(firstNotice.ContentIsEqualTo(no3));
    //        Assert.IsTrue(secondNotice.ContentIsEqualTo(no2) || secondNotice.ContentIsEqualTo(no));
    //        Assert.IsTrue(thirdNotice.ContentIsEqualTo(no2) || thirdNotice.ContentIsEqualTo(no));
    //        Assert.AreEqual(75, maxDesirabilityForFirstNotice);
    //        Assert.AreEqual(50, maxDesirabilityForSecondNotice);
    //        Assert.AreEqual(50, maxDesirabilityForThirdNotice);
    //        Assert.AreEqual(2, agentsAppliedToFirstNotice);
    //        Assert.AreEqual(2, agentsAppliedToSecondNotice);
    //        Assert.AreEqual(1, agentsAppliedToThirdNotice);
    //        Assert.IsTrue(a3.Name == agentAppliedToFirstNotice1.Name || a4.Name == agentAppliedToFirstNotice1.Name);
    //        Assert.IsTrue(a3.Name == agentAppliedToFirstNotice2.Name || a4.Name == agentAppliedToFirstNotice2.Name);
    //        Assert.IsTrue(a1.Name == agentAppliedToSecondNotice1.Name || a2.Name == agentAppliedToSecondNotice1.Name);
    //        Assert.IsTrue(a1.Name == agentAppliedToSecondNotice2.Name || a2.Name == agentAppliedToSecondNotice2.Name);
    //        Assert.IsTrue(a1.Name == agentAppliedToThirdNotice.Name || a2.Name == agentAppliedToThirdNotice.Name);
    //        Assert.AreEqual(a4.Name, agentOnFirstNotice1.Name);
    //        Assert.AreEqual(a3.Name, agentOnFirstNotice2.Name);
    //        Assert.AreEqual(a2.Name, agentOnSecondNotice.Name);
    //        Assert.AreEqual(a1.Name, agentOnThirdNotice.Name);
    //        //Assert.IsFalse(failed);
    //    }

    //    [Test]
    //    public void FindJobsForAgentsSomeAgentsIsPreferedForMultipleMultiJobs_AllJobsFilledAtStart_Success()
    //    {
    //        //Assert.Pass("test disabled");

    //        #region setup
    //        List<NodeKnowledge> nodes = new List<NodeKnowledge>() { new NodeKnowledge("n1"), new NodeKnowledge("n2"), new NodeKnowledge("n3") };

    //        NabfAgent agent1 = new NabfAgent("a1"), agent2 = new NabfAgent("a2"), agent3 = new NabfAgent("a3"), 
    //            agent4 = new NabfAgent("a4"), agent5 = new NabfAgent("a5"), agent6 = new NabfAgent("a6");

    //        Notice notice1 = new OccupyJob(2, nodes, emptyZone, 0, ID++), notice2 = new DisruptJob(2, nodes, 0, ID++), notice3 = new AttackJob(2, nodes, 0, ID++),
    //            notice4 = new RepairJob(nodes, emptyAgentName, 0, ID++);

    //        //nb.AddNotice(notice1); nb.AddNotice(notice2); nb.AddNotice(notice3); nb.AddNotice(notice4);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Occupy, 2, nodes, emptyZone, emptyAgentName, 0, out notice1);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Disrupt, 2, nodes, emptyZone, emptyAgentName, 0, out notice2);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Attack, 2, nodes, emptyZone, emptyAgentName, 0, out notice3);
    //        nb.CreateAndAddNotice(NoticeBoard.JobType.Repair, 1, nodes, emptyZone, emptyAgentName, 0, out notice4);
    //        #endregion

    //        #region agent desirability
    //        nb.ApplyToNotice(notice1, 110, agent1);
    //        nb.ApplyToNotice(notice2, 10, agent1);
    //        nb.ApplyToNotice(notice3, 11, agent1);
    //        nb.ApplyToNotice(notice4, 1111, agent1);

    //        nb.ApplyToNotice(notice1, 120, agent2);
    //        nb.ApplyToNotice(notice2, 20, agent2);
    //        nb.ApplyToNotice(notice3, 22, agent2);
    //        nb.ApplyToNotice(notice4, 2222, agent2);

    //        nb.ApplyToNotice(notice1, 130, agent3);
    //        nb.ApplyToNotice(notice2, 30, agent3);
    //        nb.ApplyToNotice(notice3, 33, agent3);

    //        nb.ApplyToNotice(notice1, 140, agent4);
    //        nb.ApplyToNotice(notice2, 40, agent4);
    //        nb.ApplyToNotice(notice3, 44, agent4);

    //        nb.ApplyToNotice(notice1, 150, agent5);
    //        nb.ApplyToNotice(notice2, 50, agent5);
    //        nb.ApplyToNotice(notice3, 55, agent5);

    //        nb.ApplyToNotice(notice1, 99999999, agent6);
    //        nb.ApplyToNotice(notice2, 99999999, agent6);
    //        nb.ApplyToNotice(notice3, 99999999, agent6);
    //        #endregion

    //        int order = 0;
    //        bool failed = false;
    //        Action<int, Notice> checkOrder = (oId, n) => { if ((order == oId)) { IsNoticeCorrect2(oId, n); order++; } else failed = true; };

    //        agent1.Register(new XmasEngineModel.Management.Trigger<ReceivedJobEvent>(evt => checkOrder(4, evt.Notice)));
    //        agent2.Register(new XmasEngineModel.Management.Trigger<ReceivedJobEvent>(evt => checkOrder(1, evt.Notice)));
    //        agent3.Register(new XmasEngineModel.Management.Trigger<ReceivedJobEvent>(evt => checkOrder(2, evt.Notice)));
    //        agent4.Register(new XmasEngineModel.Management.Trigger<ReceivedJobEvent>(evt => checkOrder(2, evt.Notice)));
    //        agent5.Register(new XmasEngineModel.Management.Trigger<ReceivedJobEvent>(evt => checkOrder(0, evt.Notice)));
    //        agent6.Register(new XmasEngineModel.Management.Trigger<ReceivedJobEvent>(evt => checkOrder(0, evt.Notice)));
            
    //        nb.FindJobsForAgents();

    //        //Assert.AreEqual(3, evtTriggered);

    //        Assert.AreEqual(notice1, notices[0]);
    //        Assert.AreEqual(notice4, notices[1]);
    //        Assert.AreEqual(notice3, notices[2]);

    //        Assert.IsTrue(listOfListOfAgents[0][0].Name == agent6.Name || listOfListOfAgents[0][0].Name == agent5.Name);
    //        Assert.IsTrue(listOfListOfAgents[0][1].Name == agent6.Name || listOfListOfAgents[0][1].Name == agent5.Name);
    //        Assert.AreEqual(listOfListOfAgents[1][0].Name, agent2.Name);
    //        Assert.IsTrue(listOfListOfAgents[2][0].Name == agent4.Name || listOfListOfAgents[2][0].Name == agent3.Name);
    //        Assert.IsTrue(listOfListOfAgents[2][1].Name == agent4.Name || listOfListOfAgents[2][1].Name == agent3.Name);

    //        Assert.AreEqual((99999999 + 150) / 2, averageDesires[0]);
    //        Assert.AreEqual(2222, averageDesires[1]);
    //        Assert.AreEqual((44 + 33) / 2, averageDesires[2]);

    //        //Assert.IsFalse(failed);
    //    }

    //    //[Test]
    //    //public void Unapply

    //    private void IsNoticeCorrect1(int evtTriggered, Notice n)
    //    {
    //        if (evtTriggered == 1)
    //        {
    //            firstNotice = n;
    //            agentsAppliedToFirstNotice = n.GetAgentsApplied().Count;
    //            maxDesirabilityForFirstNotice = n.HighestAverageDesirabilityForNotice;
    //            agentOnFirstNotice1 = n.GetTopDesireAgents()[0];
    //            agentOnFirstNotice2 = n.GetTopDesireAgents()[1];
    //            agentAppliedToFirstNotice1 = n.GetAgentsApplied()[0];
    //            agentAppliedToFirstNotice2 = n.GetAgentsApplied()[1];
    //        }
    //        if (evtTriggered == 2)
    //        {
    //            secondNotice = n;
    //            agentsAppliedToSecondNotice = n.GetAgentsApplied().Count;
    //            maxDesirabilityForSecondNotice = n.HighestAverageDesirabilityForNotice;
    //            agentOnSecondNotice = n.GetTopDesireAgents()[0];
    //            agentAppliedToSecondNotice1 = n.GetAgentsApplied()[0];
    //            agentAppliedToSecondNotice2 = n.GetAgentsApplied()[1];
    //        }
    //        if (evtTriggered == 3)
    //        {
    //            thirdNotice = n;
    //            agentsAppliedToThirdNotice = n.GetAgentsApplied().Count;
    //            maxDesirabilityForThirdNotice = n.HighestAverageDesirabilityForNotice;
    //            agentOnThirdNotice = n.GetTopDesireAgents()[0];
    //            agentAppliedToThirdNotice = n.GetAgentsApplied()[0];
    //        }
    //    }

    //    private void IsNoticeCorrect2(int evtTriggered, Notice n)
    //    {
    //        notices.Add(n);
    //        listOfListOfAgents.Add(n.GetTopDesireAgents());
    //        averageDesires.Add(n.HighestAverageDesirabilityForNotice);
    //    }

    //    private object getField(object instance, bool useBase, String name)
    //    {
    //        Type t;
    //        if (useBase)
    //            t = instance.GetType().BaseType;
    //        else
    //            t = instance.GetType();

    //        FieldInfo f = t.GetField(name, BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.IgnoreCase);
            
    //        return f.GetValue(instance);
    //    }
        
    //}
}
