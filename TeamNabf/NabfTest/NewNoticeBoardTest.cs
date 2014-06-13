using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using NUnit.Framework;
using NabfProject.NewNoticeBoardModel;
using NabfProject.AI;
using System.Reflection;
using JSLibrary.Data;
using NabfProject.KnowledgeManagerModel;
using NabfProject.Events;

namespace NabfTest.NewNoticeBoardModelTest
{
    /*
     ****Notices:
     * Create
     * Update
     * Delete
     * 
     ****Jobs(internal consistency):
     * Apply
     * Update application
     * Unapply
     * Fire
     * AssignJobs
     * 
     ****Messaging to/from agents:
     * All of the above + SendOutAllNoticesToAgent
     */

    [TestFixture]
	public class NoticeBoardTest
	{
        NewNoticeBoard nb;
        NabfAgent agent1, agent2, agent3, agent4;
        OccupyJob OccupyJob1, OccupyJob2, OccupyJob2Duplicate;
        RepairJob RepairJob1, RepairJob2, RepairJob2Duplicate;

        int DontCareInt = 1;
        string DontCareString = "";
        string DontCareString2 = "";
        List<NodeKnowledge> DontCareNodes = new List<NodeKnowledge>() { new NodeKnowledge("uniquename") };
        List<NodeKnowledge> DontCareNodes2 = new List<NodeKnowledge>() { new NodeKnowledge("moreuniquename") };
        List<Int64> ListOfKnownIDs = new List<Int64>();


        //called before each test
        [SetUp]
        public void Initialization()
        {
            nb = new NewNoticeBoard();
            agent1 = new NabfAgent("a1"); agent2 = new NabfAgent("a2"); agent3 = new NabfAgent("a3"); agent4 = new NabfAgent("a4");
        }

        #region CRUD for notices
        [Test]
		public void CreateNotice_NoDuplicateExists_Success()
		{
            #region init
            int agentsNeeded = 0;
            int jobValue = 0;
            string notNeededForOccupyJob = "";
            NewNoticeBoard.JobType jobType = NewNoticeBoard.JobType.Occupy;
            List<NodeKnowledge> whichNodesIsInvolvedInJob = new List<NodeKnowledge>() { };
            List<NodeKnowledge> whichNodesToStandOn = new List<NodeKnowledge>() { };
            #endregion

            Assert.AreEqual(0, nb.GetAllNotices().Count);

            bool createSuccess = nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, jobValue);

            Assert.True(createSuccess);
            Assert.AreEqual(1,nb.GetAllNotices().Count);
		}

        [Test]
        public void CreateNotice_ContentDuplicateExists_Failure()
        {
            #region init
            int agentsNeeded = 0;
            int jobValue = 0;
            string notNeededForOccupyJob = "";
            NewNoticeBoard.JobType jobType = NewNoticeBoard.JobType.Occupy;
            List<NodeKnowledge> whichNodesIsInvolvedInJob = new List<NodeKnowledge>() { };
            List<NodeKnowledge> whichNodesToStandOn = new List<NodeKnowledge>() { };
            #endregion

            Assert.AreEqual(0, nb.GetAllNotices().Count);

            nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, jobValue);
            Assert.AreEqual(1, nb.GetAllNotices().Count);

            bool createSuccessDuplicate = nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, jobValue);

            Assert.False(createSuccessDuplicate);
            Assert.AreEqual(1, nb.GetAllNotices().Count);
        }

        [Test]
        public void UpdateNotice_NoticeExists_Success()
        {
            #region init
            int agentsNeeded = 0;
            int jobValue = 0;
            int agentsNeededUpdated = 1;
            int jobValueUpdated = 1;
            string notNeededForOccupyJob = "";
            NewNoticeBoard.JobType jobType = NewNoticeBoard.JobType.Occupy;
            List<NodeKnowledge> whichNodesIsInvolvedInJob = new List<NodeKnowledge>() { };
            List<NodeKnowledge> whichNodesToStandOn = new List<NodeKnowledge>() { };
            #endregion

            nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, jobValue);

            Assert.AreEqual(agentsNeeded, nb.GetAllNotices().ToList()[0].AgentsNeeded);
            Assert.AreEqual(jobValue, nb.GetAllNotices().ToList()[0].Value);

            bool updateSuccess = nb.UpdateNotice(0, whichNodesIsInvolvedInJob, whichNodesToStandOn, agentsNeededUpdated, jobValueUpdated, notNeededForOccupyJob);

            Assert.True(updateSuccess);
            Assert.AreEqual(agentsNeededUpdated, nb.GetAllNotices().ToList()[0].AgentsNeeded);
            Assert.AreEqual(jobValueUpdated, nb.GetAllNotices().ToList()[0].Value);
        }
            
        [Test]
        public void UpdateNotice_NoticeDontExists_Failure()
        {
            #region init
            int agentsNeededUpdated = 1;
            int jobValueUpdated = 1;
            string notNeededForOccupyJob = "";
            List<NodeKnowledge> whichNodesIsInvolvedInJob = new List<NodeKnowledge>() { };
            List<NodeKnowledge> whichNodesToStandOn = new List<NodeKnowledge>() { };
            #endregion

            Assert.AreEqual(0, nb.GetAllNotices().Count);

            bool updateSuccess = nb.UpdateNotice(0, whichNodesIsInvolvedInJob, whichNodesToStandOn, agentsNeededUpdated, jobValueUpdated, notNeededForOccupyJob);

            Assert.False(updateSuccess);
        }

        [Test]
        public void DeleteNotice_NoticeExists_Success()
        {
            #region init
            int agentsNeeded = 0;
            int jobValue = 0;
            string notNeededForOccupyJob = "";
            NewNoticeBoard.JobType jobType = NewNoticeBoard.JobType.Occupy;
            List<NodeKnowledge> whichNodesIsInvolvedInJob = new List<NodeKnowledge>() { };
            List<NodeKnowledge> whichNodesToStandOn = new List<NodeKnowledge>() { };
            #endregion

            nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, jobValue);

            Assert.AreEqual(1, nb.GetAllNotices().Count);

            bool deleteSuccess = nb.DeleteNotice(0);

            Assert.True(deleteSuccess);
            Assert.AreEqual(0, nb.GetAllNotices().Count);
        }

        [Test]
        public void DeleteNotice_NoticeDontExists_Failure()
        {
            #region init
            int agentsNeeded = 0;
            int jobValue = 0;
            string notNeededForOccupyJob = "";
            NewNoticeBoard.JobType jobType = NewNoticeBoard.JobType.Occupy;
            List<NodeKnowledge> whichNodesIsInvolvedInJob = new List<NodeKnowledge>() { };
            List<NodeKnowledge> whichNodesToStandOn = new List<NodeKnowledge>() { };
            #endregion

            nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, jobValue);
           
            Assert.AreEqual(1, nb.GetAllNotices().Count);

            bool deleteSuccess = nb.DeleteNotice(1);//none-existing ID

            Assert.False(deleteSuccess);
            Assert.AreEqual(1, nb.GetAllNotices().Count);
        }
        #endregion

        #region Internal consistency (Jobs)
        [Test]
        public void ApplyToNotice_Simple_ApplicationAdded()
        {
            #region init
            InitNoticeBoard();
            Int64 idToApplyTo = ListOfKnownIDs[0];
            bool applySuccessful = false;
            NewNotice noticeAppliedTo = null;
            int desireOnNotice, desireAppliedWith = 1337;
            #endregion

            bool applySucceded = nb.ApplyToNotice(agent1, idToApplyTo, desireAppliedWith);

            Assert.True(applySucceded);

            foreach (NewNotice n in nb.GetAllNotices())
            {
                if (n.Id == idToApplyTo)
                {
                    if (n.GetAgentsApplied().Contains(agent1))
                        applySuccessful = true;
                    noticeAppliedTo = n;
                    break;
                }
            }

            Assert.True(applySuccessful);
            Assert.AreEqual(0, noticeAppliedTo.GetAgentsOnJob().Count);
            Assert.AreEqual(1, noticeAppliedTo.GetAgentsApplied().Count);
            noticeAppliedTo.TryGetDesirabilityOfAgent(agent1, out desireOnNotice);
            Assert.AreEqual(desireAppliedWith, desireOnNotice);
            Assert.AreEqual(NewNoticeBoard.Status.available, noticeAppliedTo.Status);
        }

        [Test]
        public void ApplyToNotice_AgentHasAlreadyApplied_overrideApplication()
        {
            #region init
            InitNoticeBoard();
            Int64 idToApplyTo = ListOfKnownIDs[0];
            bool applySuccessful = false;
            NewNotice noticeAppliedTo = null;
            int desireOnNotice, desireAppliedWith = 1337;
            #endregion

            bool applySucceded = nb.ApplyToNotice(agent1, idToApplyTo, desireAppliedWith);

            bool applySuccededSecondTime = nb.ApplyToNotice(agent1, idToApplyTo, desireAppliedWith - 1);
            foreach (NewNotice n in nb.GetAllNotices())
            {
                if (n.Id == idToApplyTo)
                {
                    if (n.GetAgentsApplied().Contains(agent1))
                        applySuccessful = true;
                    noticeAppliedTo = n;
                    break;
                }
            }
            noticeAppliedTo.TryGetDesirabilityOfAgent(agent1, out desireOnNotice);
            Assert.AreEqual(desireAppliedWith -1, desireOnNotice);

            bool applySuccededThirdTime = nb.ApplyToNotice(agent1, idToApplyTo, desireAppliedWith + 1000);

            Assert.True(applySucceded);
            Assert.True(applySuccededSecondTime);
            Assert.True(applySuccededThirdTime);

            foreach (NewNotice n in nb.GetAllNotices())
            {
                if (n.Id == idToApplyTo)
                {
                    if (n.GetAgentsApplied().Contains(agent1))
                        applySuccessful = true;
                    noticeAppliedTo = n;
                    break;
                }
            }

            Assert.True(applySuccessful);
            Assert.AreEqual(0, noticeAppliedTo.GetAgentsOnJob().Count);
            Assert.AreEqual(1, noticeAppliedTo.GetAgentsApplied().Count);
            noticeAppliedTo.TryGetDesirabilityOfAgent(agent1, out desireOnNotice);
            Assert.AreEqual(desireAppliedWith + 1000, desireOnNotice);
            Assert.AreEqual(1, noticeAppliedTo.SizeOfAgentNameToDesirabilityMapping());
            Assert.AreEqual(NewNoticeBoard.Status.available, noticeAppliedTo.Status);
        }

        [Test]
        public void ApplyToNotice_AgentAlreadyHasJob_ApplicationAdded()
        {
            Assert.True(true);
            //Assert.Pass("Test not relevant. Agents cannot apply to jobs which is taken as they are unavailable");
        }

        [Test]
        public void UnapplyToNotice_NoOneHaveTheJob_AgentRemovedFromApplyList()
        {
            #region init
            InitNoticeBoard();
            Int64 idToApplyTo = ListOfKnownIDs[0];
            NewNotice noticeAppliedTo = null;
            int desireOnNotice, desireAppliedWith = 1337;
            nb.ApplyToNotice(agent1, idToApplyTo, desireAppliedWith);
            foreach (NewNotice n in nb.GetAllNotices())
            {
                if (n.Id == idToApplyTo)
                {
                    noticeAppliedTo = n;
                    break;
                }
            }
            #endregion

            Assert.AreEqual(1, noticeAppliedTo.GetAgentsApplied().Count);
            Assert.AreEqual(NewNoticeBoard.Status.available, noticeAppliedTo.Status);

            bool unapplySuccessful = nb.UnapplyToNotice(agent1, idToApplyTo);

            Assert.True(unapplySuccessful);
            Assert.AreEqual(0, noticeAppliedTo.GetAgentsApplied().Count);
            bool agentRemovedFromDesirabilityMap = noticeAppliedTo.TryGetDesirabilityOfAgent(agent1, out desireOnNotice);
            Assert.False(agentRemovedFromDesirabilityMap);
            Assert.AreEqual(NewNoticeBoard.Status.available, noticeAppliedTo.Status);
        }
        
        [Test]
        public void UnapplyToNotice_AgentDontGotTheJob_AgentRemovedFromApplyList()//I dont have job but others do
        {
            #region init
            InitNoticeBoard();
            Int64 idToApplyTo = ListOfKnownIDs[0];
            NewNotice noticeAppliedTo = null;
            int desireOnNotice, empty, desireAppliedWith = 1337;
            nb.ApplyToNotice(agent2, idToApplyTo, desireAppliedWith);
            nb.ApplyToNotice(agent1, idToApplyTo, desireAppliedWith - 1);
            foreach (NewNotice n in nb.GetAllNotices())
            {
                if (n.Id == idToApplyTo)
                {
                    noticeAppliedTo = n;
                    break;
                }
            }
            noticeAppliedTo.AddToAgentsOnJob(agent2);
            noticeAppliedTo.Status = NewNoticeBoard.Status.unavailable;
            #endregion

            Assert.AreEqual(2, noticeAppliedTo.GetAgentsApplied().Count);
            Assert.AreEqual(1, noticeAppliedTo.GetAgentsOnJob().Count);
            Assert.AreEqual(NewNoticeBoard.Status.unavailable, noticeAppliedTo.Status);

            bool unapplySuccessful = nb.UnapplyToNotice(agent1, idToApplyTo);

            Assert.True(unapplySuccessful);
            Assert.AreEqual(1, noticeAppliedTo.GetAgentsApplied().Count);
            Assert.AreEqual(1, noticeAppliedTo.GetAgentsOnJob().Count);
            noticeAppliedTo.TryGetDesirabilityOfAgent(agent2, out desireOnNotice);
            bool removed = noticeAppliedTo.TryGetDesirabilityOfAgent(agent1, out empty);
            Assert.False(removed);
            Assert.AreEqual(desireAppliedWith, desireOnNotice);
            Assert.AreEqual(NewNoticeBoard.Status.unavailable, noticeAppliedTo.Status);
        }

        [Test]
        public void UnapplyToNotice_NoticeDontExists_failure()
        {
            #region init
            InitNoticeBoard();
            Int64 idToApplyTo = ListOfKnownIDs[0];
            #endregion

            bool unapplySuccessful = nb.UnapplyToNotice(agent1, idToApplyTo);

            Assert.False(unapplySuccessful);
        }

        [Test]
        public void UnapplyToNotice_HasTheJob_StatusSetToAvailableRestFired()
        {
            #region init
            InitNoticeBoard();
            Int64 idToApplyTo = ListOfKnownIDs[0];
            NewNotice noticeAppliedTo = null;
            int desireOnNotice, empty, desireAppliedWith = 1337;
            nb.ApplyToNotice(agent2, idToApplyTo, desireAppliedWith);
            nb.ApplyToNotice(agent1, idToApplyTo, desireAppliedWith - 1);
            foreach (NewNotice n in nb.GetAllNotices())
            {
                if (n.Id == idToApplyTo)
                {
                    noticeAppliedTo = n;
                    break;
                }
            }
            noticeAppliedTo.AddToAgentsOnJob(agent2);
            noticeAppliedTo.AddToAgentsOnJob(agent1);
            noticeAppliedTo.Status = NewNoticeBoard.Status.unavailable;
            #endregion

            Assert.AreEqual(2, noticeAppliedTo.GetAgentsApplied().Count);
            Assert.AreEqual(2, noticeAppliedTo.GetAgentsOnJob().Count);
            Assert.AreEqual(NewNoticeBoard.Status.unavailable, noticeAppliedTo.Status);

            bool unapplySuccessful = nb.UnapplyToNotice(agent1, idToApplyTo);

            Assert.True(unapplySuccessful);
            Assert.AreEqual(0, noticeAppliedTo.GetAgentsApplied().Count);
            Assert.AreEqual(0, noticeAppliedTo.GetAgentsOnJob().Count);
            Assert.AreEqual(NewNoticeBoard.Status.available, noticeAppliedTo.Status);
        }

        [Test]
        public void AssignJobs_NoJobs_NothingHappens()
        {
            bool success = nb.AssignJobs();

            Assert.False(success);
        }

        [Test]
        public void AssignJobs_JobsExistsButWithNoApplications_NothingHappens()
        {
            #region init
            InitNoticeBoard();
            Int64 idOf2AgentJob = ListOfKnownIDs[0], idOf1AgentJob = ListOfKnownIDs[2];
            int desireAppliedWith = 1337;
            nb.ApplyToNotice(agent1, idOf2AgentJob, desireAppliedWith);
            #endregion

            bool success = nb.AssignJobs();

            Assert.False(success);
        }

        [Test]
        public void AssignJobs_JobsExistsWithApplications_JobsAreAssigned()
        {
            #region init
            InitNoticeBoard();
            Int64 idOf2AgentJob = ListOfKnownIDs[0], idOf1AgentJob = ListOfKnownIDs[2], secondIdOf2AgentJob = ListOfKnownIDs[1];
            int desireAppliedWith = 1337;
            NewNotice notice1 = null, notice2 = null, notice3 = null;
            foreach (NewNotice n in nb.GetAllNotices())
            {
                if (n.Id == idOf2AgentJob)
                {
                    notice2 = n;
                }
                if (n.Id == idOf1AgentJob)
                {
                    notice1 = n;
                }
                if (n.Id == secondIdOf2AgentJob)
                {
                    notice3 = n;
                }
            }
            #endregion

            nb.ApplyToNotice(agent1, idOf2AgentJob, desireAppliedWith);
            nb.ApplyToNotice(agent2, idOf2AgentJob, desireAppliedWith);
            nb.ApplyToNotice(agent3, idOf1AgentJob, desireAppliedWith);
            nb.ApplyToNotice(agent4, secondIdOf2AgentJob, desireAppliedWith);

            bool success = nb.AssignJobs();

            Assert.True(success);

            Assert.AreEqual(NewNoticeBoard.Status.unavailable, notice1.Status);
            Assert.AreEqual(NewNoticeBoard.Status.unavailable, notice2.Status);
            Assert.AreEqual(NewNoticeBoard.Status.available, notice3.Status);

            Assert.AreEqual(1, notice1.GetAgentsApplied().Count);
            Assert.AreEqual(1, notice1.GetAgentsOnJob().Count);

            Assert.AreEqual(2, notice2.GetAgentsApplied().Count);
            Assert.AreEqual(2, notice2.GetAgentsOnJob().Count);

            Assert.AreEqual(1, notice3.GetAgentsApplied().Count);
            Assert.AreEqual(0, notice3.GetAgentsOnJob().Count);

            if (agent1.Name == notice2.GetAgentsOnJob()[0].Name)
                Assert.IsTrue(agent2.Name == notice2.GetAgentsOnJob()[1].Name);
            else if (agent1.Name == notice2.GetAgentsOnJob()[1].Name)
                Assert.IsTrue(agent2.Name == notice2.GetAgentsOnJob()[0].Name);
            else
                Assert.Fail();
        }

        [Test]
        public void AssignJobs_JobsExistsWithApplicationsButSomeAreUnavailable_SomeJobsAreAssignedRestNothingHappens()
        {
            #region init
            InitNoticeBoard();
            Int64 idOf2AgentJob = ListOfKnownIDs[0], idOf1AgentJob = ListOfKnownIDs[2];
            int desireAppliedWith = 1337;
            NewNotice notice1 = null, notice2 = null;
            foreach (NewNotice n in nb.GetAllNotices())
            {
                if (n.Id == idOf2AgentJob)
                {
                    notice2 = n;
                }
                if (n.Id == idOf1AgentJob)
                {
                    notice1 = n;
                }
            }
            #endregion

            notice1.Status = NewNoticeBoard.Status.unavailable;
            //this job will be considered in use
            //but no agents are added to it in this test, hence the later asserts for notice1

            nb.ApplyToNotice(agent1, idOf2AgentJob, desireAppliedWith);
            nb.ApplyToNotice(agent2, idOf2AgentJob, desireAppliedWith);
            nb.ApplyToNotice(agent3, idOf1AgentJob, desireAppliedWith);

            Assert.AreEqual(NewNoticeBoard.Status.unavailable, notice1.Status);
            Assert.AreEqual(0, notice1.GetAgentsOnJob().Count);

            bool success = nb.AssignJobs();

            Assert.True(success);

            Assert.AreEqual(NewNoticeBoard.Status.unavailable, notice1.Status);
            Assert.AreEqual(NewNoticeBoard.Status.unavailable, notice2.Status);

            Assert.AreEqual(1, notice1.GetAgentsApplied().Count);
            Assert.AreEqual(0, notice1.GetAgentsOnJob().Count);

            Assert.AreEqual(2, notice2.GetAgentsApplied().Count);
            Assert.AreEqual(2, notice2.GetAgentsOnJob().Count);
        }

        [Test]
        public void CalculateAverageDesireForTopContenders()
        {
            #region init
            InitNoticeBoard();
            Int64 idOf2AgentJob = ListOfKnownIDs[0], idOf1AgentJob = ListOfKnownIDs[2], secondIdOf2AgentJob = ListOfKnownIDs[1];
            int extremeDesire = 5, superDesire = 4, highDesire = 3, mediumDesire = 2, lowDesire = 1;
            List<NabfAgent> agents;
            #endregion

            nb.ApplyToNotice(agent1, idOf1AgentJob, lowDesire);
            nb.ApplyToNotice(agent1, idOf2AgentJob, mediumDesire);
            nb.ApplyToNotice(agent1, secondIdOf2AgentJob, lowDesire);

            nb.ApplyToNotice(agent2, idOf1AgentJob, superDesire);
            nb.ApplyToNotice(agent2, idOf2AgentJob, extremeDesire);
            nb.ApplyToNotice(agent2, secondIdOf2AgentJob, highDesire);

            nb.ApplyToNotice(agent3, idOf1AgentJob, highDesire);
            nb.ApplyToNotice(agent3, idOf2AgentJob, mediumDesire);
            nb.ApplyToNotice(agent3, secondIdOf2AgentJob, mediumDesire);

            nb.ApplyToNotice(agent4, idOf1AgentJob, mediumDesire);
            nb.ApplyToNotice(agent4, idOf2AgentJob, lowDesire);
            nb.ApplyToNotice(agent4, secondIdOf2AgentJob, highDesire);

            OccupyJob jobThatNeeds2Agents = (OccupyJob)nb.GetNoticeById(idOf2AgentJob);
            RepairJob jobThatNeeds1Agents = (RepairJob)nb.GetNoticeById(idOf1AgentJob);
            OccupyJob secondJobThatNeeds2Agents = (OccupyJob)nb.GetNoticeById(secondIdOf2AgentJob);

            double avgDesireJob1 = nb.CalculateAverageDesireForTopContenders(jobThatNeeds2Agents, out agents);
            Assert.IsTrue(agents.Count == 2);
            Assert.AreEqual(agent2.Name, agents[0].Name);
            Assert.IsTrue(agent1.Name == agents[1].Name || agent3.Name == agents[1].Name);
            Assert.AreEqual(((extremeDesire + mediumDesire) / 2.0), avgDesireJob1);


            double avgDesireJob2 = nb.CalculateAverageDesireForTopContenders(jobThatNeeds1Agents, out agents);
            Assert.IsTrue(agents.Count == 1);
            Assert.AreEqual(agent2.Name, agents[0].Name);
            Assert.AreEqual(superDesire, avgDesireJob2);

            double avgDesireJob3 = nb.CalculateAverageDesireForTopContenders(secondJobThatNeeds2Agents, out agents);
            Assert.AreEqual(2, agents.Count);
            if (agent2.Name == agents[0].Name)
                Assert.AreEqual(agent4.Name, agents[1].Name);
            else
            {
                Assert.AreEqual(agent4.Name, agents[0].Name);
                Assert.AreEqual(agent2.Name, agents[1].Name);
            }
            Assert.AreEqual(highDesire, avgDesireJob3);
        }

        [Test]
        public void CreateQueueSortedByAvgDesirability()
        {
            #region init
            InitNoticeBoard();
            Int64 idOf2AgentJob = ListOfKnownIDs[0], idOf1AgentJob = ListOfKnownIDs[2], secondIdOf2AgentJob = ListOfKnownIDs[1];
            int extremeDesire = 5, superDesire = 4, highDesire = 3, mediumDesire = 2, lowDesire = 1;
            double agentsNeeded2 = 2, agentsNeeded1 = 1;
            NewNotice notice1 = null, notice2 = null, notice3 = null;
            foreach (NewNotice n in nb.GetAllNotices())
            {
                if (n.Id == idOf2AgentJob)
                {
                    notice2 = n;
                }
                if (n.Id == idOf1AgentJob)
                {
                    notice1 = n;
                }
                if (n.Id == secondIdOf2AgentJob)
                {
                    notice3 = n;
                }
            }
            #endregion

            nb.ApplyToNotice(agent1, idOf1AgentJob, lowDesire);
            nb.ApplyToNotice(agent1, idOf2AgentJob, mediumDesire);
            nb.ApplyToNotice(agent1, secondIdOf2AgentJob, lowDesire);

            nb.ApplyToNotice(agent2, idOf1AgentJob, superDesire);
            nb.ApplyToNotice(agent2, idOf2AgentJob, extremeDesire);
            nb.ApplyToNotice(agent2, secondIdOf2AgentJob, highDesire);

            nb.ApplyToNotice(agent3, idOf1AgentJob, highDesire);
            nb.ApplyToNotice(agent3, idOf2AgentJob, mediumDesire);
            nb.ApplyToNotice(agent3, secondIdOf2AgentJob, mediumDesire);

            nb.ApplyToNotice(agent4, idOf1AgentJob, mediumDesire);
            nb.ApplyToNotice(agent4, idOf2AgentJob, lowDesire);
            nb.ApplyToNotice(agent4, secondIdOf2AgentJob, highDesire);

            // ----------------------
            Queue<NewNotice> jobQueue = nb.CreateQueueSortedByAvgDesirability();

            Assert.AreEqual(((0 + superDesire) / agentsNeeded1), jobQueue.Dequeue().AverageDesireFromTopContenders);
            NewNotice mostAvgDesireNotice = nb.GetNoticeById(idOf1AgentJob);
            Assert.AreEqual(1, mostAvgDesireNotice.GetAgentProspects().Count);
            Assert.AreEqual(agent2.Name, mostAvgDesireNotice.GetAgentProspects()[0].Name);

            Assert.AreEqual(((extremeDesire + mediumDesire) / 2.0), jobQueue.Dequeue().AverageDesireFromTopContenders);
            Assert.AreEqual(highDesire, jobQueue.Dequeue().AverageDesireFromTopContenders);

            // ----------------------

            nb.UnapplyToNotice(agent2, idOf2AgentJob);
            nb.UnapplyToNotice(agent2, secondIdOf2AgentJob);
            mostAvgDesireNotice.Status = NewNoticeBoard.Status.unavailable;

            jobQueue = nb.CreateQueueSortedByAvgDesirability();

            Assert.AreEqual(((highDesire + mediumDesire) / agentsNeeded2), jobQueue.Dequeue().AverageDesireFromTopContenders);
            mostAvgDesireNotice = nb.GetNoticeById(secondIdOf2AgentJob);
            Assert.AreEqual(2, mostAvgDesireNotice.GetAgentProspects().Count);
            if (agent4.Name == mostAvgDesireNotice.GetAgentProspects()[0].Name)
                Assert.AreEqual(agent3.Name, mostAvgDesireNotice.GetAgentProspects()[1].Name);
            else
            {
                Assert.AreEqual(agent3.Name, mostAvgDesireNotice.GetAgentProspects()[0].Name);
                Assert.AreEqual(agent4.Name, mostAvgDesireNotice.GetAgentProspects()[1].Name);
            }

            Assert.AreEqual(mediumDesire, jobQueue.Dequeue().AverageDesireFromTopContenders);

            // ----------------------

            nb.UnapplyToNotice(agent3, idOf2AgentJob);
            nb.UnapplyToNotice(agent4, idOf2AgentJob);
            mostAvgDesireNotice.Status = NewNoticeBoard.Status.unavailable;

            jobQueue = nb.CreateQueueSortedByAvgDesirability();

            try
            {
                jobQueue.Dequeue();
                Assert.Fail();//this fails the test if the queue is not empty which it should be as there are no more notices with enough applications which are available
            }
            catch (InvalidOperationException e)
            {
            }
        }
        
        [Test]
        public void AssignJobs_AgentDesiresConflict_JobsAreAssignedForMaximumOverallDesire()
        {
            #region init
            InitNoticeBoard();
            Int64 idOf2AgentJob = ListOfKnownIDs[0], idOf1AgentJob = ListOfKnownIDs[2], secondIdOf2AgentJob = ListOfKnownIDs[1];
            int extremeDesire = 5, superDesire = 4, highDesire = 3, mediumDesire = 2, lowDesire = 1;
            NewNotice Notice2Agents, Notice1Agent, SecondNotice2Agents;
            #endregion

            nb.ApplyToNotice(agent1, idOf1AgentJob, lowDesire);
            nb.ApplyToNotice(agent1, idOf2AgentJob, mediumDesire);
            nb.ApplyToNotice(agent1, secondIdOf2AgentJob, lowDesire);

            nb.ApplyToNotice(agent2, idOf1AgentJob, superDesire);
            nb.ApplyToNotice(agent2, idOf2AgentJob, extremeDesire);
            nb.ApplyToNotice(agent2, secondIdOf2AgentJob, highDesire);

            nb.ApplyToNotice(agent3, idOf1AgentJob, highDesire);
            nb.ApplyToNotice(agent3, idOf2AgentJob, mediumDesire);
            nb.ApplyToNotice(agent3, secondIdOf2AgentJob, mediumDesire);

            nb.ApplyToNotice(agent4, idOf1AgentJob, mediumDesire);
            nb.ApplyToNotice(agent4, idOf2AgentJob, lowDesire);
            nb.ApplyToNotice(agent4, secondIdOf2AgentJob, highDesire);

            //see document for explanation of optimal placement

            bool success = nb.AssignJobs();

            Assert.True(success);

            Assert.AreEqual(NewNoticeBoard.Status.unavailable, (Notice1Agent = nb.GetNoticeById(idOf1AgentJob)).Status);
            Assert.AreEqual(NewNoticeBoard.Status.available, (Notice2Agents = nb.GetNoticeById(idOf2AgentJob)).Status);
            Assert.AreEqual(NewNoticeBoard.Status.unavailable, (SecondNotice2Agents = nb.GetNoticeById(secondIdOf2AgentJob)).Status);

            Assert.AreEqual(2, nb.GetNoticeById(idOf1AgentJob).GetAgentsApplied().Count);
            Assert.AreEqual(1, nb.GetNoticeById(idOf1AgentJob).GetAgentsOnJob().Count);

            Assert.AreEqual(1, nb.GetNoticeById(idOf2AgentJob).GetAgentsApplied().Count);
            Assert.AreEqual(0, nb.GetNoticeById(idOf2AgentJob).GetAgentsOnJob().Count);

            Assert.AreEqual(3, nb.GetNoticeById(secondIdOf2AgentJob).GetAgentsApplied().Count);
            Assert.AreEqual(2, nb.GetNoticeById(secondIdOf2AgentJob).GetAgentsOnJob().Count);


            Assert.AreEqual(agent2.Name, nb.GetNoticeById(idOf1AgentJob).GetAgentsOnJob()[0].Name);

            if (agent3.Name == nb.GetNoticeById(secondIdOf2AgentJob).GetAgentsOnJob()[0].Name)
                Assert.AreEqual(agent4.Name, nb.GetNoticeById(secondIdOf2AgentJob).GetAgentsOnJob()[1].Name);
            else if (agent3.Name == nb.GetNoticeById(secondIdOf2AgentJob).GetAgentsOnJob()[1].Name)
                Assert.AreEqual(agent4.Name, nb.GetNoticeById(secondIdOf2AgentJob).GetAgentsOnJob()[0].Name);
            else
                Assert.Fail();
        }

        private void InitNoticeBoard()
        {
            nb.Subscribe(agent1);
            nb.Subscribe(agent2);
            nb.Subscribe(agent3);
            nb.Subscribe(agent4);
            nb.CreateNotice(NewNoticeBoard.JobType.Occupy, 2, DontCareNodes, DontCareNodes, DontCareString, DontCareInt);
            nb.CreateNotice(NewNoticeBoard.JobType.Occupy, 2, DontCareNodes2, DontCareNodes2, DontCareString, DontCareInt);
            nb.CreateNotice(NewNoticeBoard.JobType.Repair, 1, DontCareNodes, DontCareNodes, DontCareString, DontCareInt);
            nb.CreateNotice(NewNoticeBoard.JobType.Repair, 1, DontCareNodes2, DontCareNodes2, DontCareString2, DontCareInt);
            ListOfKnownIDs.Add(0);
            ListOfKnownIDs.Add(1);
            ListOfKnownIDs.Add(2);
            ListOfKnownIDs.Add(3);
        }
        #endregion

        #region Messaging
        [Test]
        public void CreateNotice_NoDuplicateExists_MsgArrived()
        {
            #region init
            int agentsNeeded = 0;
            int jobValue = 0;
            string notNeededForOccupyJob = "";
            NewNoticeBoard.JobType jobType = NewNoticeBoard.JobType.Occupy;
            List<NodeKnowledge> whichNodesIsInvolvedInJob = new List<NodeKnowledge>() { };
            List<NodeKnowledge> whichNodesToStandOn = new List<NodeKnowledge>() { };
            #endregion

            int eventFiredCounter = 0;
            agent1.Register(new XmasEngineModel.Management.Trigger<NewNoticeEvent>(evt => eventFiredCounter++));

            nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, jobValue);
            Assert.AreEqual(0,eventFiredCounter);

            nb.Subscribe(agent1);
            nb.CreateNotice(jobType, agentsNeeded, DontCareNodes, DontCareNodes, notNeededForOccupyJob, jobValue);
            Assert.AreEqual(1, eventFiredCounter);
        }
        #endregion

        #region Scenarios
        #endregion


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
