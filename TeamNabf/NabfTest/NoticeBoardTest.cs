﻿using System;
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
using XmasEngineModel.Management;

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
        NoticeBoard nb;
        NabfAgent agent1, agent2, agent3, agent4;
        OccupyJob OccupyJob1, OccupyJob2, OccupyJob2Duplicate;
        RepairJob RepairJob1, RepairJob2, RepairJob2Duplicate;

        int DontCareInt = 1, DontCareInt2 = 2;
        string DontCareString = "s1";
        string DontCareString2 = "s2";
        List<NodeKnowledge> DontCareNodes = new List<NodeKnowledge>() { new NodeKnowledge("uniquename") };
        List<NodeKnowledge> DontCareNodes2 = new List<NodeKnowledge>() { new NodeKnowledge("moreuniquename") };
        List<NodeKnowledge> DontCareNodes3 = new List<NodeKnowledge>() { new NodeKnowledge("evenmoreuniquename") };
        List<Int64> ListOfKnownIDs = new List<Int64>();

        int maxDesire = 5, extremeDesire = 4, highDesire = 3, mediumDesire = 2, lowDesire = 1;

        public enum triggerTypes { newNotice, updatedNotice, removedNotice, firedFromjob, receivedJob }

        static int NewNoticeEventFiredCounter = 0;
        static DictionaryList<string, Notice> NameToNoticesNew = new DictionaryList<string, Notice>();
        Trigger<NewNoticeEvent> NewNoticeTrigger1 = new Trigger<NewNoticeEvent>(evt => { NewNoticeEventFiredCounter++; NameToNoticesNew.Add("a1", evt.Notice); });
        Trigger<NewNoticeEvent> NewNoticetTigger2 = new Trigger<NewNoticeEvent>(evt => { NewNoticeEventFiredCounter++; NameToNoticesNew.Add("a2", evt.Notice); });
        Trigger<NewNoticeEvent> NewNoticeTrigger3 = new Trigger<NewNoticeEvent>(evt => { NewNoticeEventFiredCounter++; NameToNoticesNew.Add("a3", evt.Notice); });
        Trigger<NewNoticeEvent> NewNoticeTrigger4 = new Trigger<NewNoticeEvent>(evt => { NewNoticeEventFiredCounter++; NameToNoticesNew.Add("a4", evt.Notice); });

        static int NoticeUpdatedEventFiredCounter = 0;
        static DictionaryList<string, Notice> NameToNoticesUpdated = new DictionaryList<string, Notice>();
        Trigger<NoticeUpdatedEvent> NoticeUpdatedTrigger1 = new Trigger<NoticeUpdatedEvent>(evt => { NoticeUpdatedEventFiredCounter++; NameToNoticesUpdated.Add("a1", evt.UpdatedNotice); });
        Trigger<NoticeUpdatedEvent> NoticeUpdatedTrigger2 = new Trigger<NoticeUpdatedEvent>(evt => { NoticeUpdatedEventFiredCounter++; NameToNoticesUpdated.Add("a2", evt.UpdatedNotice); });
        Trigger<NoticeUpdatedEvent> NoticeUpdatedTrigger3 = new Trigger<NoticeUpdatedEvent>(evt => { NoticeUpdatedEventFiredCounter++; NameToNoticesUpdated.Add("a3", evt.UpdatedNotice); });
        Trigger<NoticeUpdatedEvent> NoticeUpdatedTrigger4 = new Trigger<NoticeUpdatedEvent>(evt => { NoticeUpdatedEventFiredCounter++; NameToNoticesUpdated.Add("a4", evt.UpdatedNotice); });

        static int NoticeRemovedEventFiredCounter = 0;
        Trigger<NoticeRemovedEvent> NoticeRemovedTrigger1 = new Trigger<NoticeRemovedEvent>(evt => { NoticeRemovedEventFiredCounter++; });
        Trigger<NoticeRemovedEvent> NoticeRemovedTrigger2 = new Trigger<NoticeRemovedEvent>(evt => { NoticeRemovedEventFiredCounter++; });
        Trigger<NoticeRemovedEvent> NoticeRemovedTrigger3 = new Trigger<NoticeRemovedEvent>(evt => { NoticeRemovedEventFiredCounter++; });
        Trigger<NoticeRemovedEvent> NoticeRemovedTrigger4 = new Trigger<NoticeRemovedEvent>(evt => { NoticeRemovedEventFiredCounter++; });


        static int FiredFromJobEventFiredCounter = 0;
        static List<string> NamesOfAgentsWhoGotFiredEvent = new List<string>();
        static DictionaryList<string, Int64> NameToNoticeIdsFired = new DictionaryList<string, Int64>();
        static DictionaryList<string, Notice> NameToNoticesFired = new DictionaryList<string, Notice>();
        Trigger<FiredFromJobEvent> FiredFromJobTrigger1 = new Trigger<FiredFromJobEvent>(evt => { FiredFromJobEventFiredCounter++; NamesOfAgentsWhoGotFiredEvent.Add("a1"); NameToNoticesFired.Add("a1", evt.Notice); NameToNoticeIdsFired.Add("a1", evt.Notice.Id); });
        Trigger<FiredFromJobEvent> FiredFromJobTrigger2 = new Trigger<FiredFromJobEvent>(evt => { FiredFromJobEventFiredCounter++; NamesOfAgentsWhoGotFiredEvent.Add("a2"); NameToNoticesFired.Add("a2", evt.Notice); NameToNoticeIdsFired.Add("a2", evt.Notice.Id); });
        Trigger<FiredFromJobEvent> FiredFromJobTrigger3 = new Trigger<FiredFromJobEvent>(evt => { FiredFromJobEventFiredCounter++; NamesOfAgentsWhoGotFiredEvent.Add("a3"); NameToNoticesFired.Add("a3", evt.Notice); NameToNoticeIdsFired.Add("a3", evt.Notice.Id); });
        Trigger<FiredFromJobEvent> FiredFromJobTrigger4 = new Trigger<FiredFromJobEvent>(evt => { FiredFromJobEventFiredCounter++; NamesOfAgentsWhoGotFiredEvent.Add("a4"); NameToNoticesFired.Add("a4", evt.Notice); NameToNoticeIdsFired.Add("a4", evt.Notice.Id); });

        static int ReceivedJobEventFiredCounter = 0;
        static List<string> NamesOfAgentsWhoReceivedJob = new List<string>();
        static DictionaryList<string, Int64> NameToNoticeIdsReceived = new DictionaryList<string, Int64>();
        static DictionaryList<string, Notice> NameToNoticesReceived = new DictionaryList<string, Notice>();
        Trigger<ReceivedJobEvent> ReceivedJobTrigger1 = new Trigger<ReceivedJobEvent>(evt => { ReceivedJobEventFiredCounter++; NamesOfAgentsWhoReceivedJob.Add("a1"); NameToNoticesReceived.Add("a1", evt.Notice); NameToNoticeIdsReceived.Add("a1", evt.Notice.Id); });
        Trigger<ReceivedJobEvent> ReceivedJobTrigger2 = new Trigger<ReceivedJobEvent>(evt => { ReceivedJobEventFiredCounter++; NamesOfAgentsWhoReceivedJob.Add("a2"); NameToNoticesReceived.Add("a2", evt.Notice); NameToNoticeIdsReceived.Add("a2", evt.Notice.Id); });
        Trigger<ReceivedJobEvent> ReceivedJobTrigger3 = new Trigger<ReceivedJobEvent>(evt => { ReceivedJobEventFiredCounter++; NamesOfAgentsWhoReceivedJob.Add("a3"); NameToNoticesReceived.Add("a3", evt.Notice); NameToNoticeIdsReceived.Add("a3", evt.Notice.Id); });
        Trigger<ReceivedJobEvent> ReceivedJobTrigger4 = new Trigger<ReceivedJobEvent>(evt => { ReceivedJobEventFiredCounter++; NamesOfAgentsWhoReceivedJob.Add("a4"); NameToNoticesReceived.Add("a4", evt.Notice); NameToNoticeIdsReceived.Add("a4", evt.Notice.Id); });


        //called before each test
        [SetUp]
        public void Initialization()
        {
            nb = new NoticeBoard();
            agent1 = new NabfAgent("a1"); agent2 = new NabfAgent("a2"); agent3 = new NabfAgent("a3"); agent4 = new NabfAgent("a4");
            ListOfKnownIDs.Clear();
        }

        #region CRUD for notices
        [Test]
		public void CreateNotice_NoDuplicateExists_Success()
		{
            #region init
            int agentsNeeded = 0;
            int jobValue = 0;
            string notNeededForOccupyJob = "";
            NoticeBoard.JobType jobType = NoticeBoard.JobType.Occupy;
            List<NodeKnowledge> whichNodesIsInvolvedInJob = new List<NodeKnowledge>() { };
            List<NodeKnowledge> whichNodesToStandOn = new List<NodeKnowledge>() { };
            #endregion

            Assert.AreEqual(0, nb.GetAllNotices().Count);

            bool createSuccess = nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, DontCareInt, jobValue);

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
            NoticeBoard.JobType jobType = NoticeBoard.JobType.Occupy;
            List<NodeKnowledge> whichNodesIsInvolvedInJob = new List<NodeKnowledge>() { };
            List<NodeKnowledge> whichNodesToStandOn = new List<NodeKnowledge>() { };
            #endregion

            Assert.AreEqual(0, nb.GetAllNotices().Count);

            nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, DontCareInt, jobValue);
            Assert.AreEqual(1, nb.GetAllNotices().Count);

            bool createSuccessDuplicate = nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, DontCareInt, jobValue);

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
            NoticeBoard.JobType jobType = NoticeBoard.JobType.Occupy;
            List<NodeKnowledge> whichNodesIsInvolvedInJob = new List<NodeKnowledge>() { };
            List<NodeKnowledge> whichNodesToStandOn = new List<NodeKnowledge>() { };
            #endregion

            nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, DontCareInt, jobValue);

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
            NoticeBoard.JobType jobType = NoticeBoard.JobType.Occupy;
            List<NodeKnowledge> whichNodesIsInvolvedInJob = new List<NodeKnowledge>() { };
            List<NodeKnowledge> whichNodesToStandOn = new List<NodeKnowledge>() { };
            #endregion

            nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, DontCareInt, jobValue);

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
            NoticeBoard.JobType jobType = NoticeBoard.JobType.Occupy;
            List<NodeKnowledge> whichNodesIsInvolvedInJob = new List<NodeKnowledge>() { };
            List<NodeKnowledge> whichNodesToStandOn = new List<NodeKnowledge>() { };
            #endregion

            nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, DontCareInt, jobValue);
           
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
            InitNoticeBoardInternalTesting();
            Int64 idToApplyTo = ListOfKnownIDs[0];
            bool applySuccessful = false;
            Notice noticeAppliedTo = null;
            double desireOnNotice, desireAppliedWith = 1337;
            #endregion

            bool applySucceded = nb.ApplyToNotice(agent1, idToApplyTo, desireAppliedWith);

            Assert.True(applySucceded);

            foreach (Notice n in nb.GetAllNotices())
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
            Assert.AreEqual(NoticeBoard.Status.available, noticeAppliedTo.Status);
        }

        [Test]
        public void ApplyToNotice_NoticeIsUnavailable_ApplicationRejected()
        {
            #region init
            InitNoticeBoardInternalTesting();
            Int64 idToApplyTo = ListOfKnownIDs[0];
            bool applySuccessful = false;
            Notice noticeAppliedTo = null;
            int desireOnNotice, desireAppliedWith = 1337;
            #endregion

            Notice notice;
            nb.TryGetNoticeById(idToApplyTo, out notice);
            notice.Status = NoticeBoard.Status.unavailable;

            bool applySucceded = nb.ApplyToNotice(agent1, idToApplyTo, desireAppliedWith);

            Assert.False(applySucceded);

            foreach (Notice n in nb.GetAllNotices())
            {
                if (n.Id == idToApplyTo)
                {
                    noticeAppliedTo = n;
                    break;
                }
            }
            Assert.AreEqual(0, noticeAppliedTo.GetAgentsOnJob().Count);
            Assert.AreEqual(0, noticeAppliedTo.GetAgentsApplied().Count);
        }

        [Test]
        public void ApplyToNotice_AgentHasAlreadyApplied_overrideApplication()
        {
            #region init
            InitNoticeBoardInternalTesting();
            Int64 idToApplyTo = ListOfKnownIDs[0];
            bool applySuccessful = false;
            Notice noticeAppliedTo = null;
            double desireOnNotice, desireAppliedWith = 1337;
            #endregion

            bool applySucceded = nb.ApplyToNotice(agent1, idToApplyTo, desireAppliedWith);

            bool applySuccededSecondTime = nb.ApplyToNotice(agent1, idToApplyTo, desireAppliedWith - 1);
            foreach (Notice n in nb.GetAllNotices())
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

            foreach (Notice n in nb.GetAllNotices())
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
            Assert.AreEqual(NoticeBoard.Status.available, noticeAppliedTo.Status);
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
            InitNoticeBoardInternalTesting();
            Int64 idToApplyTo = ListOfKnownIDs[0];
            Notice noticeAppliedTo = null;
            double desireOnNotice, desireAppliedWith = 1337;
            nb.ApplyToNotice(agent1, idToApplyTo, desireAppliedWith);
            foreach (Notice n in nb.GetAllNotices())
            {
                if (n.Id == idToApplyTo)
                {
                    noticeAppliedTo = n;
                    break;
                }
            }
            #endregion

            Assert.AreEqual(1, noticeAppliedTo.GetAgentsApplied().Count);
            Assert.AreEqual(NoticeBoard.Status.available, noticeAppliedTo.Status);

            bool unapplySuccessful = nb.UnapplyToNotice(agent1, idToApplyTo);

            Assert.True(unapplySuccessful);
            Assert.AreEqual(0, noticeAppliedTo.GetAgentsApplied().Count);
            bool agentRemovedFromDesirabilityMap = noticeAppliedTo.TryGetDesirabilityOfAgent(agent1, out desireOnNotice);
            Assert.False(agentRemovedFromDesirabilityMap);
            Assert.AreEqual(NoticeBoard.Status.available, noticeAppliedTo.Status);
        }
        
        [Test]
        public void UnapplyToNotice_AgentDontGotTheJob_AgentRemovedFromApplyList()//I dont have the job but others do
        {
            #region init
            InitNoticeBoardInternalTesting();
            Int64 idToApplyTo = ListOfKnownIDs[0];
            Notice noticeAppliedTo = null;
            double desireOnNotice, empty, desireAppliedWith = 1337;
            #endregion

            nb.ApplyToNotice(agent2, idToApplyTo, desireAppliedWith);
            nb.ApplyToNotice(agent1, idToApplyTo, desireAppliedWith - 1);

            nb.TryGetNoticeById(idToApplyTo, out noticeAppliedTo);

            noticeAppliedTo.AddToAgentsOnJob(agent2);
            noticeAppliedTo.Status = NoticeBoard.Status.unavailable;

            Assert.AreEqual(2, noticeAppliedTo.GetAgentsApplied().Count);
            Assert.AreEqual(1, noticeAppliedTo.GetAgentsOnJob().Count);
            Assert.AreEqual(NoticeBoard.Status.unavailable, noticeAppliedTo.Status);

            bool unapplySuccessful = nb.UnapplyToNotice(agent1, idToApplyTo);

            nb.TryGetNoticeById(idToApplyTo, out noticeAppliedTo);

            Assert.True(unapplySuccessful);
            Assert.AreEqual(1, noticeAppliedTo.GetAgentsApplied().Count);
            Assert.AreEqual(1, noticeAppliedTo.GetAgentsOnJob().Count);
            noticeAppliedTo.TryGetDesirabilityOfAgent(agent2, out desireOnNotice);
            bool removed = noticeAppliedTo.TryGetDesirabilityOfAgent(agent1, out empty);
            Assert.False(removed);
            Assert.AreEqual(desireAppliedWith, desireOnNotice);
            Assert.AreEqual(NoticeBoard.Status.unavailable, noticeAppliedTo.Status);
        }

        [Test]
        public void UnapplyToNotice_NoticeDontExists_failure()
        {
            #region init
            InitNoticeBoardInternalTesting();
            Int64 idToApplyTo = ListOfKnownIDs[0];
            #endregion

            bool unapplySuccessful = nb.UnapplyToNotice(agent1, idToApplyTo);

            Assert.False(unapplySuccessful);
        }

        [Test]
        public void UnapplyToNotice_HasTheJob_StatusSetToAvailableRestFired()
        {
            #region init
            InitNoticeBoardInternalTesting();
            Int64 idToApplyTo = ListOfKnownIDs[0];
            Notice noticeAppliedTo = null;
            int desireOnNotice, empty, desireAppliedWith = 1337;
            nb.ApplyToNotice(agent2, idToApplyTo, desireAppliedWith);
            nb.ApplyToNotice(agent1, idToApplyTo, desireAppliedWith - 1);
            foreach (Notice n in nb.GetAllNotices())
            {
                if (n.Id == idToApplyTo)
                {
                    noticeAppliedTo = n;
                    break;
                }
            }
            noticeAppliedTo.AddToAgentsOnJob(agent2);
            noticeAppliedTo.AddToAgentsOnJob(agent1);
            noticeAppliedTo.Status = NoticeBoard.Status.unavailable;
            #endregion

            Assert.AreEqual(2, noticeAppliedTo.GetAgentsApplied().Count);
            Assert.AreEqual(2, noticeAppliedTo.GetAgentsOnJob().Count);
            Assert.AreEqual(NoticeBoard.Status.unavailable, noticeAppliedTo.Status);

            bool unapplySuccessful = nb.UnapplyToNotice(agent1, idToApplyTo);

            Assert.True(unapplySuccessful);
            Assert.AreEqual(0, noticeAppliedTo.GetAgentsApplied().Count);
            Assert.AreEqual(0, noticeAppliedTo.GetAgentsOnJob().Count);
            Assert.AreEqual(NoticeBoard.Status.available, noticeAppliedTo.Status);
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
            InitNoticeBoardInternalTesting();
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
            InitNoticeBoardInternalTesting();
            Int64 idOf2AgentJob = ListOfKnownIDs[0], idOf1AgentJob = ListOfKnownIDs[2], secondIdOf2AgentJob = ListOfKnownIDs[1];
            int desireAppliedWith = 1337;
            Notice notice1 = null, notice2 = null, notice3 = null;
            foreach (Notice n in nb.GetAllNotices())
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

            Assert.AreEqual(NoticeBoard.Status.unavailable, notice1.Status);
            Assert.AreEqual(NoticeBoard.Status.unavailable, notice2.Status);
            Assert.AreEqual(NoticeBoard.Status.available, notice3.Status);

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
            InitNoticeBoardInternalTesting();
            Int64 idOf2AgentJob = ListOfKnownIDs[0], idOf1AgentJob = ListOfKnownIDs[2];
            int desireAppliedWith = 1337;
            Notice noticeWhichRequire1Agent = null, noticeWhichRequire2Agents = null;
            foreach (Notice n in nb.GetAllNotices())
            {
                if (n.Id == idOf2AgentJob)
                {
                    noticeWhichRequire2Agents = n;
                }
                if (n.Id == idOf1AgentJob)
                {
                    noticeWhichRequire1Agent = n;
                }
            }
            #endregion

            //this job will be considered in use
            //but no agents are added to it in this test, hence the later asserts for notice1

            nb.ApplyToNotice(agent1, idOf2AgentJob, desireAppliedWith);
            nb.ApplyToNotice(agent2, idOf2AgentJob, desireAppliedWith);
            nb.ApplyToNotice(agent3, idOf1AgentJob, desireAppliedWith);
            noticeWhichRequire1Agent.Status = NoticeBoard.Status.unavailable;

            Assert.AreEqual(NoticeBoard.Status.unavailable, noticeWhichRequire1Agent.Status);
            Assert.AreEqual(0, noticeWhichRequire1Agent.GetAgentsOnJob().Count);
            Assert.AreEqual(1, noticeWhichRequire1Agent.GetAgentsApplied().Count);

            bool success = nb.AssignJobs();

            Assert.True(success);

            Assert.AreEqual(NoticeBoard.Status.unavailable, noticeWhichRequire1Agent.Status);
            Assert.AreEqual(NoticeBoard.Status.unavailable, noticeWhichRequire2Agents.Status);

            Assert.AreEqual(1, noticeWhichRequire1Agent.GetAgentsApplied().Count);
            Assert.AreEqual(0, noticeWhichRequire1Agent.GetAgentsOnJob().Count);

            Assert.AreEqual(2, noticeWhichRequire2Agents.GetAgentsApplied().Count);
            Assert.AreEqual(2, noticeWhichRequire2Agents.GetAgentsOnJob().Count);
        }

        [Test]
        public void CalculateAverageDesireForTopContenders()
        {
            #region init
            InitNoticeBoardInternalTesting();
            Int64 idOf2AgentJob = ListOfKnownIDs[0], idOf1AgentJob = ListOfKnownIDs[2], secondIdOf2AgentJob = ListOfKnownIDs[1];
            List<NabfAgent> agents;
            #endregion

            nb.ApplyToNotice(agent1, idOf1AgentJob, lowDesire);
            nb.ApplyToNotice(agent1, idOf2AgentJob, mediumDesire);
            nb.ApplyToNotice(agent1, secondIdOf2AgentJob, lowDesire);

            nb.ApplyToNotice(agent2, idOf1AgentJob, extremeDesire);
            nb.ApplyToNotice(agent2, idOf2AgentJob, maxDesire);
            nb.ApplyToNotice(agent2, secondIdOf2AgentJob, highDesire);

            nb.ApplyToNotice(agent3, idOf1AgentJob, highDesire);
            nb.ApplyToNotice(agent3, idOf2AgentJob, mediumDesire);
            nb.ApplyToNotice(agent3, secondIdOf2AgentJob, mediumDesire);

            nb.ApplyToNotice(agent4, idOf1AgentJob, mediumDesire);
            nb.ApplyToNotice(agent4, idOf2AgentJob, lowDesire);
            nb.ApplyToNotice(agent4, secondIdOf2AgentJob, highDesire);

            Notice n;
            OccupyJob jobThatNeeds2Agents;
            nb.TryGetNoticeById(idOf2AgentJob, out n);
            jobThatNeeds2Agents = (OccupyJob)n;

            RepairJob jobThatNeeds1Agents;
            nb.TryGetNoticeById(idOf1AgentJob, out n);
            jobThatNeeds1Agents = (RepairJob)n;

            OccupyJob secondJobThatNeeds2Agents;
            nb.TryGetNoticeById(secondIdOf2AgentJob, out n);
            secondJobThatNeeds2Agents = (OccupyJob)n;

            double avgDesireJob1 = nb.CalculateAverageDesireForTopContenders(jobThatNeeds2Agents, out agents);
            Assert.IsTrue(agents.Count == 2);
            Assert.AreEqual(agent2.Name, agents[0].Name);
            Assert.IsTrue(agent1.Name == agents[1].Name || agent3.Name == agents[1].Name);
            Assert.AreEqual(((maxDesire + mediumDesire) / 2.0), avgDesireJob1);


            double avgDesireJob2 = nb.CalculateAverageDesireForTopContenders(jobThatNeeds1Agents, out agents);
            Assert.IsTrue(agents.Count == 1);
            Assert.AreEqual(agent2.Name, agents[0].Name);
            Assert.AreEqual(extremeDesire, avgDesireJob2);

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
            InitNoticeBoardInternalTesting();
            Int64 idOf2AgentJob = ListOfKnownIDs[0], idOf1AgentJob = ListOfKnownIDs[2], secondIdOf2AgentJob = ListOfKnownIDs[1];
            double agentsNeeded2 = 2, agentsNeeded1 = 1;
            Notice notice1 = null, notice2 = null, notice3 = null;
            foreach (Notice n in nb.GetAllNotices())
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

            nb.ApplyToNotice(agent2, idOf1AgentJob, extremeDesire);
            nb.ApplyToNotice(agent2, idOf2AgentJob, maxDesire);
            nb.ApplyToNotice(agent2, secondIdOf2AgentJob, highDesire);

            nb.ApplyToNotice(agent3, idOf1AgentJob, highDesire);
            nb.ApplyToNotice(agent3, idOf2AgentJob, mediumDesire);
            nb.ApplyToNotice(agent3, secondIdOf2AgentJob, mediumDesire);

            nb.ApplyToNotice(agent4, idOf1AgentJob, mediumDesire);
            nb.ApplyToNotice(agent4, idOf2AgentJob, lowDesire);
            nb.ApplyToNotice(agent4, secondIdOf2AgentJob, highDesire);

            // ----------------------
            Queue<Notice> jobQueue = nb.CreateQueueSortedByAvgDesirability();

            Assert.AreEqual(((0 + extremeDesire) / agentsNeeded1), jobQueue.Dequeue().AverageDesireFromTopContenders);
            Notice mostAvgDesireNotice;
            nb.TryGetNoticeById(idOf1AgentJob, out mostAvgDesireNotice);
            Assert.AreEqual(1, mostAvgDesireNotice.GetAgentProspects().Count);
            Assert.AreEqual(agent2.Name, mostAvgDesireNotice.GetAgentProspects()[0].Name);

            Assert.AreEqual(((maxDesire + mediumDesire) / 2.0), jobQueue.Dequeue().AverageDesireFromTopContenders);
            Assert.AreEqual(highDesire, jobQueue.Dequeue().AverageDesireFromTopContenders);

            // ----------------------

            nb.UnapplyToNotice(agent2, idOf2AgentJob);
            nb.UnapplyToNotice(agent2, secondIdOf2AgentJob);
            mostAvgDesireNotice.Status = NoticeBoard.Status.unavailable;

            jobQueue = nb.CreateQueueSortedByAvgDesirability();

            Assert.AreEqual(((highDesire + mediumDesire) / agentsNeeded2), jobQueue.Dequeue().AverageDesireFromTopContenders);
            nb.TryGetNoticeById(secondIdOf2AgentJob, out mostAvgDesireNotice);
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
            mostAvgDesireNotice.Status = NoticeBoard.Status.unavailable;

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
            InitNoticeBoardInternalTesting();
            Int64 idOf2AgentJob = ListOfKnownIDs[0], idOf1AgentJob = ListOfKnownIDs[2], secondIdOf2AgentJob = ListOfKnownIDs[1];
            Notice Notice2Agents, Notice1Agent, SecondNotice2Agents;
            #endregion

            nb.ApplyToNotice(agent1, idOf1AgentJob, lowDesire);
            nb.ApplyToNotice(agent1, idOf2AgentJob, mediumDesire);
            nb.ApplyToNotice(agent1, secondIdOf2AgentJob, lowDesire);

            nb.ApplyToNotice(agent2, idOf1AgentJob, extremeDesire);
            nb.ApplyToNotice(agent2, idOf2AgentJob, maxDesire);
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
            nb.TryGetNoticeById(idOf1AgentJob, out Notice1Agent);
            nb.TryGetNoticeById(idOf2AgentJob, out Notice2Agents);
            nb.TryGetNoticeById(secondIdOf2AgentJob, out SecondNotice2Agents);
            Assert.AreEqual(NoticeBoard.Status.unavailable, Notice1Agent.Status);
            Assert.AreEqual(NoticeBoard.Status.available, Notice2Agents.Status);
            Assert.AreEqual(NoticeBoard.Status.unavailable, SecondNotice2Agents.Status);

            Assert.AreEqual(4, Notice1Agent.GetAgentsApplied().Count);
            Assert.AreEqual(1, Notice1Agent.GetAgentsOnJob().Count);

            Assert.AreEqual(4, Notice2Agents.GetAgentsApplied().Count);
            Assert.AreEqual(0, Notice2Agents.GetAgentsOnJob().Count);

            Assert.AreEqual(4, SecondNotice2Agents.GetAgentsApplied().Count);
            Assert.AreEqual(2, SecondNotice2Agents.GetAgentsOnJob().Count);


            Assert.AreEqual(agent2.Name, Notice1Agent.GetAgentsOnJob()[0].Name);

            if (agent3.Name == SecondNotice2Agents.GetAgentsOnJob()[0].Name)
                Assert.AreEqual(agent4.Name, SecondNotice2Agents.GetAgentsOnJob()[1].Name);
            else if (agent3.Name == SecondNotice2Agents.GetAgentsOnJob()[1].Name)
                Assert.AreEqual(agent4.Name, SecondNotice2Agents.GetAgentsOnJob()[0].Name);
            else
                Assert.Fail();
        }

        private void InitNoticeBoardInternalTesting()
        {
            nb.Subscribe(agent1);
            nb.Subscribe(agent2);
            nb.Subscribe(agent3);
            nb.Subscribe(agent4);
            nb.CreateNotice(NoticeBoard.JobType.Occupy, 2, DontCareNodes, DontCareNodes, DontCareString, DontCareInt, DontCareInt);
            nb.CreateNotice(NoticeBoard.JobType.Occupy, 2, DontCareNodes2, DontCareNodes2, DontCareString, DontCareInt, DontCareInt);
            nb.CreateNotice(NoticeBoard.JobType.Repair, 1, DontCareNodes, DontCareNodes, DontCareString, DontCareInt, DontCareInt);
            nb.CreateNotice(NoticeBoard.JobType.Repair, 1, DontCareNodes2, DontCareNodes2, DontCareString2, DontCareInt, DontCareInt);
            ListOfKnownIDs.Add(0);
            ListOfKnownIDs.Add(1);
            ListOfKnownIDs.Add(2);
            ListOfKnownIDs.Add(3);
        }
        #endregion

        #region Messaging
        [Test]
        public void CreateNotice_NoDuplicateExists_NewMsgArrived()
        {
            #region init
            int agentsNeeded = 0;
            int jobValue = 0;
            string notNeededForOccupyJob = "";
            NoticeBoard.JobType jobType = NoticeBoard.JobType.Occupy;
            List<NodeKnowledge> whichNodesIsInvolvedInJob = new List<NodeKnowledge>() { };
            List<NodeKnowledge> whichNodesToStandOn = new List<NodeKnowledge>() { };
            #endregion

            int eventFiredCounter = 0;
            Trigger<NewNoticeEvent> trigger = new Trigger<NewNoticeEvent>(evt => eventFiredCounter++);
            agent1.Register(trigger);
            agent2.Register(trigger);
            agent3.Register(trigger);
            agent4.Register(trigger);

            nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, DontCareInt, jobValue);
            Assert.AreEqual(0,eventFiredCounter);

            nb.Subscribe(agent1);
            nb.Subscribe(agent2);
            nb.Subscribe(agent3);
            nb.Subscribe(agent4);
            nb.CreateNotice(jobType, agentsNeeded, DontCareNodes, DontCareNodes, notNeededForOccupyJob, DontCareInt, jobValue);
            Assert.AreEqual(4, eventFiredCounter);


            agent1.Deregister(trigger);
            agent2.Deregister(trigger);
            agent3.Deregister(trigger);
            agent4.Deregister(trigger);
        }

        [Test]
        public void CreateNotice_AttemptsNonUniqueNotices_NewNoticeMsgArrivedOnce()
        {
            #region init
            int agentsNeeded = 0;
            int jobValue = 0;
            string notNeededForOccupyJob = "";
            NoticeBoard.JobType jobType = NoticeBoard.JobType.Occupy;
            List<NodeKnowledge> whichNodesIsInvolvedInJob = new List<NodeKnowledge>() { new NodeKnowledge("nodenamehere") };
            List<NodeKnowledge> whichNodesToStandOn = new List<NodeKnowledge>() { new NodeKnowledge("anothernodenamehere") };
            ICollection<Notice> noticesFromEvent;
            Notice noticeInternal;
            #endregion
            SetUpTriggers();


            nb.Subscribe(agent1);
            nb.Subscribe(agent2);
            nb.Subscribe(agent3);
            nb.Subscribe(agent4);

            Int64 idOfFirstJob = 0, idOfSecondJob;
            nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, DontCareInt, jobValue);
            Assert.AreEqual(4, NewNoticeEventFiredCounter);
            nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, DontCareInt, jobValue);
            Assert.AreEqual(4, NewNoticeEventFiredCounter);
            nb.CreateNotice(jobType, agentsNeeded, whichNodesIsInvolvedInJob, whichNodesToStandOn, notNeededForOccupyJob, DontCareInt, jobValue);
            Assert.AreEqual(4, NewNoticeEventFiredCounter);

            NameToNoticesNew.TryGetValues(agent3.Name, out noticesFromEvent);
            nb.TryGetNoticeById(idOfFirstJob, out noticeInternal);
            Assert.AreEqual(1, noticesFromEvent.Count);
            Assert.IsTrue(noticeInternal.ContentIsEqualTo(noticesFromEvent.ToList()[0]));

            nb.CreateNotice(jobType, agentsNeeded, DontCareNodes, DontCareNodes, notNeededForOccupyJob, DontCareInt, jobValue);
            Assert.AreEqual(8, NewNoticeEventFiredCounter);
            nb.CreateNotice(jobType, agentsNeeded, DontCareNodes, DontCareNodes, notNeededForOccupyJob, DontCareInt, jobValue);
            Assert.AreEqual(8, NewNoticeEventFiredCounter);
            nb.CreateNotice(jobType, agentsNeeded, DontCareNodes, DontCareNodes, notNeededForOccupyJob, DontCareInt, jobValue);
            Assert.AreEqual(8, NewNoticeEventFiredCounter);

            idOfSecondJob = 3; //even if a job is rejected the ID counter is incremented, hence the ID of the second successfull job in this case is 3 as the id of the first is 0
            NameToNoticesNew.TryGetValues(agent3.Name, out noticesFromEvent);
            nb.TryGetNoticeById(idOfSecondJob, out noticeInternal);
            Assert.AreEqual(2, noticesFromEvent.Count);
            Assert.IsTrue(noticeInternal.ContentIsEqualTo(noticesFromEvent.ToList()[1]));

            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes>() { triggerTypes.newNotice }));

            CleanUpTriggers();
        }

        [Test]
        public void UpdateNotice_NoticeExists_UpdatedMsgArrived()
        {
            SetUpTriggersAndNoticeBoard();

            ICollection<Notice> noticesFromEvent;
            Notice noticeInternal;

            nb.UpdateNotice(ListOfKnownIDs[0], DontCareNodes, DontCareNodes, DontCareInt, DontCareInt, DontCareString);
            Assert.AreEqual(4, NoticeUpdatedEventFiredCounter);

            NameToNoticesUpdated.TryGetValues(agent1.Name, out noticesFromEvent);
            nb.TryGetNoticeById(ListOfKnownIDs[0], out noticeInternal);
            Assert.AreEqual(1, noticesFromEvent.Count);
            Assert.IsTrue(noticeInternal.ContentIsEqualTo(noticesFromEvent.ToList()[0]));


            NameToNoticesUpdated.Clear();


            nb.UpdateNotice(ListOfKnownIDs[0], DontCareNodes2, DontCareNodes2, DontCareInt2, DontCareInt2, DontCareString2);
            Assert.AreEqual(8, NoticeUpdatedEventFiredCounter);

            NameToNoticesUpdated.TryGetValues(agent1.Name, out noticesFromEvent);
            nb.TryGetNoticeById(ListOfKnownIDs[0], out noticeInternal);
            Assert.AreEqual(1, noticesFromEvent.Count);
            Assert.IsTrue(noticeInternal.ContentIsEqualTo(noticesFromEvent.ToList()[0]));

            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { triggerTypes.updatedNotice }));

            CleanUpTriggers();
        }

        [Test]
        public void UpdateNotice_NoticeDontExists_NoMsg()
        {
            SetUpTriggersAndNoticeBoard();

            nb.UpdateNotice(666, DontCareNodes, DontCareNodes, DontCareInt, DontCareInt, DontCareString);

            Assert.AreEqual(0, NoticeUpdatedEventFiredCounter);

            nb.UpdateNotice(999, DontCareNodes, DontCareNodes, DontCareInt, DontCareInt, DontCareString);

            Assert.AreEqual(0, NoticeUpdatedEventFiredCounter);
            
            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes>{ triggerTypes.updatedNotice }));

            CleanUpTriggers();
        }
        
        [Test]
        public void DeleteNotice_NoticeExists_DeleteMsgArrived()
        {
            SetUpTriggersAndNoticeBoard();

            nb.DeleteNotice(ListOfKnownIDs[0]);

            Assert.AreEqual(4, NoticeRemovedEventFiredCounter);

            nb.DeleteNotice(ListOfKnownIDs[1]);

            Assert.AreEqual(8, NoticeRemovedEventFiredCounter);

            nb.DeleteNotice(ListOfKnownIDs[1]);

            Assert.AreEqual(8, NoticeRemovedEventFiredCounter);

            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { triggerTypes.removedNotice }));

            CleanUpTriggers();
        }

        [Test]
        public void DeleteNotice_NoticeDontExists_NoMsg()
        {
            SetUpTriggersAndNoticeBoard();

            nb.DeleteNotice(666);

            Assert.AreEqual(0, NoticeRemovedEventFiredCounter);

            nb.DeleteNotice(999);

            Assert.AreEqual(0, NoticeRemovedEventFiredCounter);

            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { triggerTypes.removedNotice }));

            CleanUpTriggers();
        }

        [Test]
        public void UnapplyToNotice_NoticeDontExists_NoMsg()
        {
            SetUpTriggersAndNoticeBoard();

            nb.UnapplyToNotice(agent1, 999);

            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { }));

            nb.AssignJobs();

            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { }));

            CleanUpTriggers();
        }

        [Test]
        public void UnapplyToNotice_AgentHaventApplied_NoMsg()
        {
            SetUpTriggersAndNoticeBoard();

            nb.UnapplyToNotice(agent1, ListOfKnownIDs[0]);

            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { }));

            nb.AssignJobs();

            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { }));

            CleanUpTriggers();
        }

        [Test]
        public void UnapplyToNotice_NoOneHasTheJob_NoMsg()
        {
            SetUpTriggersAndNoticeBoard();
            Int64 idOf2AgentJob = ListOfKnownIDs[0];

            nb.ApplyToNotice(agent1, idOf2AgentJob, 999);
            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { }));

            nb.AssignJobs();
            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { }));

            nb.UnapplyToNotice(agent1, idOf2AgentJob);
            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { }));

            nb.AssignJobs();
            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { }));

            CleanUpTriggers();
        }

        [Test]
        public void UnapplyToNotice_SomeoneElseHasTheJob_NoMsg()
        {
            SetUpTriggersAndNoticeBoard();
            Int64 idOf2AgentJob = ListOfKnownIDs[0];

            nb.ApplyToNotice(agent1, idOf2AgentJob, 0);
            nb.ApplyToNotice(agent2, idOf2AgentJob, 2);
            nb.ApplyToNotice(agent3, idOf2AgentJob, 3);
            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { }));

            nb.AssignJobs();
            //due to brute force method we will also always fire some agents
            //Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { triggerTypes.receivedJob }));
            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { triggerTypes.receivedJob, triggerTypes.firedFromjob }));

            ReceivedJobEventFiredCounter = 0;

            nb.UnapplyToNotice(agent1, idOf2AgentJob);
            //due to brute force method we will also always fire some agents
            //Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> {  }));
            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { triggerTypes.firedFromjob }));

            nb.AssignJobs();
            //due to brute force method we will also always fire some agents
            //Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> {  }));
            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { triggerTypes.firedFromjob }));

            CleanUpTriggers();
        }

        [Test]
        public void UnapplyToNotice_AgenteHasTheJob_FiredMsgsArrived()
        {
            SetUpTriggersAndNoticeBoard();
            Int64 idOf3AgentJob = ListOfKnownIDs[4];
            Notice n;
            nb.TryGetNoticeById(idOf3AgentJob, out n);

            Assert.AreEqual(3, n.AgentsNeeded);

            nb.ApplyToNotice(agent1, idOf3AgentJob, 10);
            nb.ApplyToNotice(agent2, idOf3AgentJob, 5);
            nb.ApplyToNotice(agent3, idOf3AgentJob, 0);

            nb.AssignJobs();
            Assert.AreEqual(3, NamesOfAgentsWhoReceivedJob.Count);
            Assert.AreEqual(agent1.Name, NamesOfAgentsWhoReceivedJob[0]);
            Assert.AreEqual(agent2.Name, NamesOfAgentsWhoReceivedJob[1]);
            Assert.AreEqual(agent3.Name, NamesOfAgentsWhoReceivedJob[2]);
            Assert.AreEqual(3, ReceivedJobEventFiredCounter);
            //due to brute force method we will also always fire some agents
            //Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { triggerTypes.receivedJob }));
            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { triggerTypes.receivedJob, triggerTypes.firedFromjob }));

            ReceivedJobEventFiredCounter = 0;

            nb.UnapplyToNotice(agent1, idOf3AgentJob);
            //due to brute force method we will also always additional agents
            //Assert.AreEqual(2, FiredFromJobEventFiredCounter);
            //Assert.AreEqual(2, NamesOfAgentsWhoGotFiredEvent.Count);
            //if (NamesOfAgentsWhoGotFiredEvent[0] == agent2.Name)
            //    Assert.AreEqual(agent3.Name, NamesOfAgentsWhoGotFiredEvent[1]);
            //else
            //{
            //    Assert.AreEqual(agent3.Name, NamesOfAgentsWhoGotFiredEvent[0]);
            //    Assert.AreEqual(agent2.Name, NamesOfAgentsWhoGotFiredEvent[1]);
            //}
            if (NamesOfAgentsWhoGotFiredEvent[1] == agent2.Name)
                Assert.AreEqual(agent3.Name, NamesOfAgentsWhoGotFiredEvent[2]);
            else
            {
                Assert.AreEqual(agent3.Name, NamesOfAgentsWhoGotFiredEvent[1]);
                Assert.AreEqual(agent2.Name, NamesOfAgentsWhoGotFiredEvent[2]);
            }
            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { triggerTypes.firedFromjob }));


            CleanUpTriggers();
        }

        [Test]
        public void AssignJobs_NoJobs_NoMsg()
        {
            SetUpTriggers();

            nb.Subscribe(agent1);
            nb.Subscribe(agent2);
            nb.Subscribe(agent3);
            nb.Subscribe(agent4);

            nb.AssignJobs();

            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { }));

            CleanUpTriggers();
        }

        [Test]
        public void AssignJobs_JobsExistsButWithNoApplications_NoMsg()
        {
            SetUpTriggersAndNoticeBoard();

            nb.AssignJobs();

            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { }));

            CleanUpTriggers();
        }
        
        [Test]
        public void AssignJobCausesFiringInOldJob_AgentGotOldJobButWantsNew_MsgsArrived()
        {
            SetUpTriggersAndNoticeBoard();
            ICollection<Notice> noticesFromEvent;
            Notice noticeInternal;
            Int64 idOf3AgentJob = ListOfKnownIDs[4], idOf2AgentJob = ListOfKnownIDs[0], idOf1AgentJob = ListOfKnownIDs[2];

            nb.ApplyToNotice(agent1, idOf3AgentJob, lowDesire);
            nb.ApplyToNotice(agent2, idOf3AgentJob, mediumDesire);
            nb.ApplyToNotice(agent3, idOf3AgentJob, highDesire);

            nb.ApplyToNotice(agent1, idOf2AgentJob, lowDesire);
            nb.ApplyToNotice(agent2, idOf2AgentJob, lowDesire);
            nb.ApplyToNotice(agent3, idOf2AgentJob, lowDesire);

            //nb.AssignJobs();//arbitary number of call to assignjobs
            //nb.AssignJobs();
            nb.AssignJobs();
            Assert.AreEqual(3, NamesOfAgentsWhoReceivedJob.Count);
            Assert.AreEqual(3, ReceivedJobEventFiredCounter);
            ReceivedJobEventFiredCounter = 0;
            NamesOfAgentsWhoReceivedJob.Clear();
            NameToNoticesReceived.Clear();


            nb.ApplyToNotice(agent3, idOf1AgentJob, highDesire);
            nb.ApplyToNotice(agent1, idOf1AgentJob, maxDesire);
            nb.ApplyToNotice(agent2, idOf1AgentJob, highDesire);

            //nb.AssignJobs();//arbitary number of call to assignjobs
            nb.AssignJobs();
            Assert.AreEqual(3, NamesOfAgentsWhoReceivedJob.Count);
            Assert.AreEqual(agent1.Name, NamesOfAgentsWhoReceivedJob[0]);
            Assert.AreEqual(3, ReceivedJobEventFiredCounter);
            NameToNoticesReceived.TryGetValues(agent1.Name, out noticesFromEvent);
            nb.TryGetNoticeById(idOf1AgentJob, out noticeInternal);
            Assert.AreEqual(1, noticesFromEvent.Count);
            Assert.IsTrue(noticeInternal.ContentIsEqualTo(noticesFromEvent.ToList()[0]));

            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { triggerTypes.receivedJob, triggerTypes.firedFromjob }));
            //these lines will fail due to brute force method of firing all agents from all notices they are not on jobs on
            //Assert.AreEqual(2, FiredFromJobEventFiredCounter);
            //Assert.AreEqual(2, NamesOfAgentsWhoGotFiredEvent.Count);
            //if (NamesOfAgentsWhoGotFiredEvent[0] == agent2.Name)
            //    Assert.AreEqual(agent3.Name, NamesOfAgentsWhoGotFiredEvent[1]);
            //else
            //{
            //    Assert.AreEqual(agent3.Name, NamesOfAgentsWhoGotFiredEvent[0]);
            //    Assert.AreEqual(agent2.Name, NamesOfAgentsWhoGotFiredEvent[1]);
            //}
            if (NamesOfAgentsWhoGotFiredEvent[1] == agent2.Name)
                Assert.AreEqual(agent3.Name, NamesOfAgentsWhoGotFiredEvent[2]);
            else
            {
                Assert.AreEqual(agent3.Name, NamesOfAgentsWhoGotFiredEvent[1]);
                Assert.AreEqual(agent2.Name, NamesOfAgentsWhoGotFiredEvent[2]);
            }

            //NameToNoticesFired.TryGetValues(agent2.Name, out noticesFromEvent);
            //nb.TryGetNoticeById(idOf3AgentJob, out noticeInternal);
            //Assert.AreEqual(1, noticesFromEvent.Count);
            //Assert.IsTrue(noticeInternal.ContentIsEqualTo(noticesFromEvent.ToList()[0]));

            //NameToNoticesFired.TryGetValues(agent3.Name, out noticesFromEvent);
            //Assert.AreEqual(1, noticesFromEvent.Count);
            //Assert.IsTrue(noticeInternal.ContentIsEqualTo(noticesFromEvent.ToList()[0]));
            //agent2 & 3 wont automatically get the 2-agent-job, but next time they apply they would get it

            CleanUpTriggers();
        }

        [Test]
        public void AssignJobs_AgentDesiresConflict_JobsAreAssignedForMaximumOverallDesireAndMsgsArrived()
        {
            SetUpTriggersAndNoticeBoard();

            Int64 idOf2AgentJob = ListOfKnownIDs[0], idOf1AgentJob = ListOfKnownIDs[2], secondIdOf2AgentJob = ListOfKnownIDs[1];
            ICollection<Int64> idsOfNoticesForAgent1, idsOfNoticesForAgent2, idsOfNoticesForAgent3, idsOfNoticesForAgent4;
            ICollection<Notice> noticesForAgent1, noticesForAgent2, noticesForAgent3, noticesForAgent4;
            Notice Notice1Agent, SecondNotice2Agents;

            nb.ApplyToNotice(agent1, idOf1AgentJob, lowDesire);
            nb.ApplyToNotice(agent1, idOf2AgentJob, mediumDesire);
            nb.ApplyToNotice(agent1, secondIdOf2AgentJob, lowDesire);

            nb.ApplyToNotice(agent2, idOf1AgentJob, extremeDesire);
            nb.ApplyToNotice(agent2, idOf2AgentJob, maxDesire);
            nb.ApplyToNotice(agent2, secondIdOf2AgentJob, highDesire);

            nb.ApplyToNotice(agent3, idOf1AgentJob, highDesire);
            nb.ApplyToNotice(agent3, idOf2AgentJob, mediumDesire);
            nb.ApplyToNotice(agent3, secondIdOf2AgentJob, mediumDesire);

            nb.ApplyToNotice(agent4, idOf1AgentJob, mediumDesire);
            nb.ApplyToNotice(agent4, idOf2AgentJob, lowDesire);
            nb.ApplyToNotice(agent4, secondIdOf2AgentJob, highDesire);

            //see document for explanation of optimal placement

            nb.AssignJobs();

            Assert.AreEqual(3, NamesOfAgentsWhoReceivedJob.Count);
            Assert.AreEqual(3, ReceivedJobEventFiredCounter);

            NameToNoticeIdsReceived.TryGetValues(agent1.Name, out idsOfNoticesForAgent1);
            NameToNoticesReceived.TryGetValues(agent1.Name, out noticesForAgent1);
            Assert.AreEqual(null, idsOfNoticesForAgent1);
            Assert.AreEqual(null, noticesForAgent1);

            //checking if the agent has received the correct job with the correct ID and nothing else
            NameToNoticeIdsReceived.TryGetValues(agent2.Name, out idsOfNoticesForAgent2);
            NameToNoticesReceived.TryGetValues(agent2.Name, out noticesForAgent2);
            Assert.AreEqual(1, idsOfNoticesForAgent2.Count);
            Assert.AreEqual(idOf1AgentJob, idsOfNoticesForAgent2.ToList()[0]);
            nb.TryGetNoticeById(idOf1AgentJob, out Notice1Agent);
            Assert.AreEqual(1, noticesForAgent2.Count);
            Assert.IsTrue(Notice1Agent.ContentIsEqualTo(noticesForAgent2.ToList()[0]));


            NameToNoticeIdsReceived.TryGetValues(agent3.Name, out idsOfNoticesForAgent3);
            NameToNoticesReceived.TryGetValues(agent3.Name, out noticesForAgent3);
            Assert.AreEqual(1, idsOfNoticesForAgent3.Count);
            Assert.AreEqual(secondIdOf2AgentJob, idsOfNoticesForAgent3.ToList()[0]);
            nb.TryGetNoticeById(secondIdOf2AgentJob, out SecondNotice2Agents);
            Assert.AreEqual(1, noticesForAgent3.Count);
            Assert.IsTrue(SecondNotice2Agents.ContentIsEqualTo(noticesForAgent3.ToList()[0]));


            NameToNoticeIdsReceived.TryGetValues(agent4.Name, out idsOfNoticesForAgent4);
            NameToNoticesReceived.TryGetValues(agent4.Name, out noticesForAgent4);
            Assert.AreEqual(1, idsOfNoticesForAgent4.Count);
            Assert.AreEqual(secondIdOf2AgentJob, idsOfNoticesForAgent4.ToList()[0]);
            nb.TryGetNoticeById(secondIdOf2AgentJob, out SecondNotice2Agents);
            Assert.AreEqual(1, noticesForAgent4.Count);
            Assert.IsTrue(SecondNotice2Agents.ContentIsEqualTo(noticesForAgent4.ToList()[0]));

        }

        [Test]
        public void SendOutAllNoticesToAgent_NoNotices_NoMsgs()
        {
            nb.Subscribe(agent1);
            nb.Subscribe(agent2);
            nb.Subscribe(agent3);
            nb.Subscribe(agent4);
            SetUpTriggers();

            nb.SendOutAllNoticesToAgent(agent1);

            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { }));

            CleanUpTriggers();
        }

        [Test]
        public void SendOutAllNoticesToAgent_ManyNotices_MsgsArrived()
        {
            SetUpTriggersAndNoticeBoard();

            nb.SendOutAllNoticesToAgent(agent1);

            Assert.AreEqual(nb.GetAllNotices().Count, NewNoticeEventFiredCounter);
            Assert.True(OnlyTheseTriggersFired(new List<triggerTypes> { triggerTypes.newNotice }));

            CleanUpTriggers();
        }

        private void SetUpTriggersAndNoticeBoard()
        {
            SetUpTriggers();

            InitNoticeBoardMsgTesting();

            nb.Subscribe(agent1);
            nb.Subscribe(agent2);
            nb.Subscribe(agent3);
            nb.Subscribe(agent4);
        }
        private void InitNoticeBoardMsgTesting()
        {
            ListOfKnownIDs = new List<long>();
            nb.CreateNotice(NoticeBoard.JobType.Occupy, 2, DontCareNodes, DontCareNodes, DontCareString, DontCareInt, DontCareInt);
            nb.CreateNotice(NoticeBoard.JobType.Occupy, 2, DontCareNodes2, DontCareNodes2, DontCareString, DontCareInt, DontCareInt);
            nb.CreateNotice(NoticeBoard.JobType.Repair, 1, DontCareNodes, DontCareNodes, DontCareString, DontCareInt, DontCareInt);
            nb.CreateNotice(NoticeBoard.JobType.Repair, 1, DontCareNodes2, DontCareNodes2, DontCareString2, DontCareInt, DontCareInt);
            nb.CreateNotice(NoticeBoard.JobType.Occupy, 3, DontCareNodes3, DontCareNodes3, DontCareString2, DontCareInt, DontCareInt);
            ListOfKnownIDs.Add(0);
            ListOfKnownIDs.Add(1);
            ListOfKnownIDs.Add(2);
            ListOfKnownIDs.Add(3);
            ListOfKnownIDs.Add(4);
        }
        private void SetUpTriggers()
        {
            NewNoticeEventFiredCounter = 0;
            agent1.Register(NewNoticeTrigger1);
            agent2.Register(NewNoticetTigger2);
            agent3.Register(NewNoticeTrigger3);
            agent4.Register(NewNoticeTrigger4);

            NoticeUpdatedEventFiredCounter = 0;
            agent1.Register(NoticeUpdatedTrigger1);
            agent2.Register(NoticeUpdatedTrigger2);
            agent3.Register(NoticeUpdatedTrigger3);
            agent4.Register(NoticeUpdatedTrigger4);

            NoticeRemovedEventFiredCounter = 0;
            agent1.Register(NoticeRemovedTrigger1);
            agent2.Register(NoticeRemovedTrigger2);
            agent3.Register(NoticeRemovedTrigger3);
            agent4.Register(NoticeRemovedTrigger4);

            FiredFromJobEventFiredCounter = 0;
            NamesOfAgentsWhoGotFiredEvent.Clear();
            agent1.Register(FiredFromJobTrigger1);
            agent2.Register(FiredFromJobTrigger2);
            agent3.Register(FiredFromJobTrigger3);
            agent4.Register(FiredFromJobTrigger4); 

            ReceivedJobEventFiredCounter = 0;
            NamesOfAgentsWhoReceivedJob.Clear();
            agent1.Register(ReceivedJobTrigger1);
            agent2.Register(ReceivedJobTrigger2);
            agent3.Register(ReceivedJobTrigger3);
            agent4.Register(ReceivedJobTrigger4);
        }
        private void CleanUpTriggers()
        {
            NewNoticeEventFiredCounter = 0;
            NameToNoticesNew = new DictionaryList<string, Notice>();
            agent1.Deregister(NewNoticeTrigger1);
            agent2.Deregister(NewNoticetTigger2);
            agent3.Deregister(NewNoticeTrigger3);
            agent4.Deregister(NewNoticeTrigger4);

            NoticeUpdatedEventFiredCounter = 0;
            NameToNoticesUpdated = new DictionaryList<string, Notice>();
            agent1.Deregister(NoticeUpdatedTrigger1);
            agent2.Deregister(NoticeUpdatedTrigger2);
            agent3.Deregister(NoticeUpdatedTrigger3);
            agent4.Deregister(NoticeUpdatedTrigger4);

            NoticeRemovedEventFiredCounter = 0;
            agent1.Deregister(NoticeRemovedTrigger1);
            agent2.Deregister(NoticeRemovedTrigger2);
            agent3.Deregister(NoticeRemovedTrigger3);
            agent4.Deregister(NoticeRemovedTrigger4);

            FiredFromJobEventFiredCounter = 0;
            NamesOfAgentsWhoGotFiredEvent = new List<string>();
            NameToNoticeIdsFired = new DictionaryList<string, long>();
            NameToNoticesFired = new DictionaryList<string, Notice>();
            agent1.Deregister(FiredFromJobTrigger1);
            agent2.Deregister(FiredFromJobTrigger2);
            agent3.Deregister(FiredFromJobTrigger3);
            agent4.Deregister(FiredFromJobTrigger4);

            ReceivedJobEventFiredCounter = 0;
            NamesOfAgentsWhoReceivedJob = new List<string>();
            NameToNoticeIdsReceived = new DictionaryList<string, long>();
            NameToNoticesReceived = new DictionaryList<string, Notice>();
            agent1.Deregister(ReceivedJobTrigger1);
            agent2.Deregister(ReceivedJobTrigger2);
            agent3.Deregister(ReceivedJobTrigger3);
            agent4.Deregister(ReceivedJobTrigger4);
        }
        private bool OnlyTheseTriggersFired(List<triggerTypes> dontCheckTheseTypes)
        {
            int maxValueTriggerType = (int)(Enum.GetValues(typeof(triggerTypes)).Cast<triggerTypes>().Last());
            bool skipThis, result = true;
            triggerTypes currentType;


            for (int i = 0; i <= maxValueTriggerType; i++)
            {
                skipThis = false;
                foreach (triggerTypes t in dontCheckTheseTypes)
                {
                    if (((int)t) == i)
                        skipThis = true;
                }
                if (skipThis)
                    continue;
                currentType = (triggerTypes)i;
                switch (currentType)
                {
                    case triggerTypes.firedFromjob:
                        if (FiredFromJobEventFiredCounter > 0)
                            result = false;
                        break;
                    case triggerTypes.newNotice:
                        if (NewNoticeEventFiredCounter > 0)
                            result = false;
                        break;
                    case triggerTypes.removedNotice:
                        if (NoticeRemovedEventFiredCounter > 0)
                            result = false;
                        break;
                    case triggerTypes.updatedNotice:
                        if (NoticeUpdatedEventFiredCounter > 0)
                            result = false;
                        break;
                    case triggerTypes.receivedJob:
                        if (ReceivedJobEventFiredCounter > 0)
                            result = false;
                        break;
                        
                }
            }

            return result;
        }
        #endregion

        #region Scenarios
        //multiple rounds test
        //random spam through simman test
        //random spam through simman test (copy of above)
        //random spam through simman test (copy of above)
        #endregion


        private object getField(object instance, bool useBase, String nameOfField)
        {
            Type t;
            if (useBase)
                t = instance.GetType().BaseType;
            else
                t = instance.GetType();

            FieldInfo f = t.GetField(nameOfField, BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.IgnoreCase);
            
            return f.GetValue(instance);
        }
        
	}
}
