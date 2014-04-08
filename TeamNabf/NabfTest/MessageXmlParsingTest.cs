using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using NUnit.Framework;
using NabfProject.AI;
using System.IO;
using NabfProject.ServerMessages;
using System.Xml.Serialization;
using System.Xml;
using System.Xml.Linq;

namespace NabfTest
{
    [TestFixture]
    public class MessageXmlParsingTest
    { 
        ServerCommunication servCom;
        MemoryStream memStream;
        StreamReader reader;
        StreamWriter writer;

        [SetUp]
        public void Init()
        {
            memStream = new MemoryStream();
            memStream.Position = 0;
            reader = new StreamReader(memStream);
            writer = new StreamWriter(memStream);
            servCom = new ServerCommunication(reader, writer);
        }

        [Test]
        public void SendMessageAuthentication_Connected_XMLmessageSent()
        {
            string username = "a1";
            string password = "1";

            AuthenticationRequestMessage message = new AuthenticationRequestMessage(username, password);
            servCom.SeralizePacket(message);
            memStream.Position = 0;
            string data = reader.ReadToEnd();
            data = data.Remove(data.Length - 1);

            string actual = XDocument.Parse(data).ToString();
            string expected = XDocument.Parse("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>"
                                +"<message type=\"auth-request\">"
                                +"<authentication password=\""+password+"\" username=\""+username+"\"/>"
                                + "</message>").ToString();

            Assert.AreEqual(expected, actual);
        }

        [Test]
        public void SendMessageAction_Connected_XMLmessageSent2()
        {
            string actionType = "goto";
            string actionParam = "vertex1";
            int id = 1;

            ActionMessage message = new ActionMessage(id, actionType, actionParam);
            servCom.SeralizePacket(message);
            memStream.Position = 0;
            string data = reader.ReadToEnd();
            data = data.Remove(data.Length - 1);

            string actual = XDocument.Parse(data).ToString();
            string expected = XDocument.Parse("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                                                +"<message type=\"action\">"
                                                +"<action id=\""+id+"\" type=\""+actionType+"\" param=\""+actionParam+"\"/>"
                                                +"</message>").ToString();

            Assert.AreEqual(expected, actual);
        }

        [Test]
        public void ReceiveMessageAuthentication_Connected_XmlMessageReceived()
        {
            string result = "ok";

            writer.Write("<message timestamp=\"1297263037617\" type=\"auth-response\">"
                            + "<authentication result=\"" + result + "\"/>"
                            + "</message>");
            writer.Flush();
            writer.BaseStream.WriteByte(0);
            writer.BaseStream.Flush();

            

            memStream.Position = 0;
            reader = new StreamReader(memStream);
            servCom = new ServerCommunication(reader, writer);
            
            InternalReceiveMessage message = servCom.DeserializeMessage();

            Assert.IsInstanceOf<AuthenticationResponseMessage>(message);
            Assert.IsTrue(((AuthenticationResponseMessage)message).Response == ServerResponseTypes.Success);
        }

        [Test]
        public void ReceiveMessageSimStart_Connected_XmlMessageReceived()
        {
            string edges = "47";
            string id = "0";
            string steps = "500";
            string vertices = "20";
            string role = "Explorer";

            writer.Write("<message timestamp=\"1297263004607\" type=\"sim-start\">"
                            + "<simulation edges=\""+edges+"\" id=\""+id+"\" steps=\""+steps+"\" vertices=\""+vertices+"\" role=\""+role+"\"/>"
                            + "</message>");

            writer.Flush();
            writer.BaseStream.WriteByte(0);
            writer.BaseStream.Flush();

            memStream.Position = 0;
            
            reader = new StreamReader(memStream);
            servCom = new ServerCommunication(reader, writer);

            InternalReceiveMessage message = servCom.DeserializeMessage();

			var simStart = (SimStartMessage)message;

            Assert.IsInstanceOf<SimStartMessage>(message);
            Assert.IsTrue(simStart.Edges == Convert.ToInt32(edges));
            Assert.IsTrue(simStart.Id == Convert.ToInt32(id));
            Assert.IsTrue(simStart.Steps == Convert.ToInt32(steps));
            Assert.IsTrue(simStart.Vertices == Convert.ToInt32(vertices));
            Assert.IsTrue(simStart.Role == role);

        }

        [Test]
        public void ReceiveMessageSimEnd_Connected_XmlMessageReceived()
        {
            string ranking = "2";
            string score = "9";

            writer.Write("<message timestamp=\"1297269179279\" type=\"sim-end\">"
                            + "<sim-result ranking=\""+ranking+"\" score=\""+score+"\"/>"
                            + "</message>");

            writer.Flush();
            writer.BaseStream.WriteByte(0);
            writer.BaseStream.Flush();

            memStream.Position = 0;

            reader = new StreamReader(memStream);
            servCom = new ServerCommunication(reader, writer);

            InternalReceiveMessage message = servCom.DeserializeMessage();

             

            Assert.IsInstanceOf<SimEndMessage>(message);

			var simEnd = (SimEndMessage)message;

            Assert.IsTrue(simEnd.Ranking == Convert.ToInt32(ranking));
            Assert.IsTrue(simEnd.Score == Convert.ToInt32(score));
        }

        [Test]
        public void ReceiveMessageBye_Connected_XmlMessageReceived()
        {
            writer.Write("<message timestamp=\"1204978760555\" type=\"bye\"/>");

            writer.Flush();
            writer.BaseStream.WriteByte(0);
            writer.BaseStream.Flush();

            memStream.Position = 0;

            reader = new StreamReader(memStream);
            servCom = new ServerCommunication(reader, writer);

            InternalReceiveMessage message = servCom.DeserializeMessage();

            Assert.IsInstanceOf<ByeMessage>(message);
            Assert.IsTrue(((ByeMessage)message).Response == ServerResponseTypes.Success);
        }

        [Test]
        public void ReceiveMessageRequestAction_Connected_XmlMessageReceived()
        {
            long timestamp = 1297263230578;
            long perception_deadline = 1297263232578;
            int perception_id = 201;
            int simulation_step = 200;
            int self_energy = 19;
            int self_health = 9;
            string self_lastAction = "skip";
            string self_lastActionParam = "";
            string self_lastActionResult = "successful";
            int self_maxEnergy = 19;
            int self_maxEnergyDisabled = 9;
            int self_maxHealth = 9;
            string self_position = "vertex4";
            int self_strength = 5;
            int self_visRange = 5;
            int self_zoneScore = 27;
            int team_lastStepScore = 27;
            int team_money = 1;
            int team_score = 4270;
            int team_zonesScore = 26;
            string achievement_name = "area20";
            string visibleVertex_name = "vertex19";
            string visibleVertex_team = "none";
            string visibleEdge_node1 = "vertex0";
            string visibleEdge_node2 = "vertex11";
            string visibleEntity_status = "normal";
            int probedVertex_value = 4;
            int surveyedEdge_weight = 2;
            int inspectedEntity_maxHealth = 9;

            writer.Write("<message timestamp=\"" + timestamp + "\" type=\"request-action\">"
                            + "<perception deadline=\""+perception_deadline+"\" id=\""+perception_id+"\">"
                            + "<simulation step=\""+simulation_step+"\"/>"
                            + "<self energy=\""+self_energy+"\" health=\""+self_health+"\" lastAction=\""+self_lastAction+"\" " 
                            + "lastActionParam=\""+self_lastActionParam+"\" lastActionResult=\""+self_lastActionResult+"\" maxEnergy=\""+self_maxEnergy+"\" "
                            + "maxEnergyDisabled=\""+self_maxEnergyDisabled+"\" maxHealth=\""+self_maxHealth+"\" position=\""+self_position+"\" "
                            + "strength=\""+self_strength+"\" visRange=\""+self_visRange+"\" zoneScore=\""+self_zoneScore+"\"/>"
                            + "<team lastStepScore=\""+team_lastStepScore+"\" money=\""+team_money+"\" score=\""+team_score+"\" zonesScore=\""+team_zonesScore+"\">"
                            + "<achievements>"
                            + "<achievement name=\""+achievement_name+"\"/>"
                            + "</achievements>"
                            + "</team>"
                            + "<visibleVertices>"
                            + "<visibleVertex name=\""+visibleVertex_name+"\" team=\""+visibleVertex_team+"\"/>"
                            + "</visibleVertices>"
                            + "<visibleEdges>"
                            + "<visibleEdge node1=\""+visibleEdge_node1+"\" node2=\""+visibleEdge_node2+"\"/>"
                            + "</visibleEdges>"
                            + "<visibleEntities>"
                            + "<visibleEntity name=\"b5\" team=\"B\" node=\"vertex0\" "
                            + "status=\""+visibleEntity_status+"\"/>"
                            + "</visibleEntities>"
                            + "<probedVertices>"
                            + "<probedVertex name=\"vertex18\" value=\""+probedVertex_value+"\"/>"
                            + "</probedVertices>"
                            + "<surveyedEdges>"
                            + "<surveyedEdge node1=\"vertex3\" node2=\"vertex7\" weight=\""+surveyedEdge_weight+"\"/>"
                            + "</surveyedEdges>"
                            + "<inspectedEntities>"
                            + "<inspectedEntity energy=\"8\" health=\"9\" maxEnergy=\"8\" "
                            + "maxHealth=\""+inspectedEntity_maxHealth+"\" name=\"b5\" node=\"vertex10\" role=\"role2\" "
                            + "strength=\"6\" team=\"B\" visRange=\"2\"/>"
                            + "</inspectedEntities>"
                            + "</perception>"
                            + "</message>");

            writer.Flush();
            writer.BaseStream.WriteByte(0);
            writer.BaseStream.Flush();

            memStream.Position = 0;

            reader = new StreamReader(memStream);
            servCom = new ServerCommunication(reader, writer);

            ReceiveMessage packet = (ReceiveMessage)servCom.DeserializePacket();
            InternalReceiveMessage message = packet.Message;

            RequestActionMessage requestActionData = (RequestActionMessage) message;
            PerceptionMessage perceptionData = (PerceptionMessage) requestActionData.Response;
            SimulationMessage simulationData = (SimulationMessage)perceptionData.Elements[0];
            SelfMessage selfData = (SelfMessage)perceptionData.Elements[1];
            TeamMessage teamData = (TeamMessage)perceptionData.Elements[2];
            AchievementsMessage achievementsData = (AchievementsMessage)teamData.Achievements;
            List<InternalReceiveMessage> achievementData = (List<InternalReceiveMessage>)achievementsData.AchievementList;
            VisibleVertexMessage visibleVertexData = (VisibleVertexMessage)((VisibleVerticesMessage)perceptionData.Elements[3]).VisibleVertices[0];
            VisibleEdgeMessage visibleEdgeData = (VisibleEdgeMessage)((VisibleEdgesMessage)perceptionData.Elements[4]).VisibleEdges[0];
            VisibleEntityMessage visibleEntityData = (VisibleEntityMessage)((VisibleEntitiesMessage)perceptionData.Elements[5]).VisibleEntities[0];
            ProbedVertexMessage probedVertexData = (ProbedVertexMessage)((ProbedVerticesMessage)perceptionData.Elements[6]).ProbedVertices[0];
            SurveyedEdgeMessage surveyedEdgeData = (SurveyedEdgeMessage)((SurveyedEdgesMessage)perceptionData.Elements[7]).SurveyedEdges[0];
            InspectedEntityMessage inspectedEntityData = (InspectedEntityMessage)((InspectedEntitiesMessage)perceptionData.Elements[8]).InspectedEntities[0];

            Assert.AreEqual("perception", perceptionData.MessageName);
            Assert.AreEqual("simulation", simulationData.MessageName);
            Assert.AreEqual("self", selfData.MessageName);
            Assert.AreEqual("team", teamData.MessageName);

            Assert.AreEqual(timestamp, packet.Timestamp);
            Assert.AreEqual(perception_deadline, perceptionData.Deadline);
            Assert.AreEqual(perception_id, perceptionData.Id);
            Assert.AreEqual(simulation_step, simulationData.Step);
            Assert.AreEqual(self_energy, selfData.Energy);
            Assert.AreEqual(self_lastActionParam, selfData.LastActionParam);
            Assert.AreEqual(team_lastStepScore, teamData.LastStepScore);
            Assert.AreEqual(achievement_name, ((AchievementMessage) achievementData[0]).Name);
            Assert.AreEqual(visibleVertex_name, visibleVertexData.Name);
            Assert.AreEqual(visibleEdge_node1, visibleEdgeData.Node1);
            Assert.AreEqual(visibleEntity_status, visibleEntityData.Status);
            Assert.AreEqual(probedVertex_value, probedVertexData.Value);
            Assert.AreEqual(surveyedEdge_weight, surveyedEdgeData.Weight);
            Assert.AreEqual(inspectedEntity_maxHealth, inspectedEntityData.MaxHealth);

        }

        [Test]
        public void ReceiveMessageRequestActionNoAchievements_Connected_XmlMessageReceived()
        {
            long timestamp = 1297263230578;
            long perception_deadline = 1297263232578;
            int perception_id = 201;
            int simulation_step = 200;
            int self_energy = 19;
            int self_health = 9;
            string self_lastAction = "skip";
            string self_lastActionParam = "";
            string self_lastActionResult = "successful";
            int self_maxEnergy = 19;
            int self_maxEnergyDisabled = 9;
            int self_maxHealth = 9;
            string self_position = "vertex4";
            int self_strength = 5;
            int self_visRange = 5;
            int self_zoneScore = 27;
            int team_lastStepScore = 27;
            int team_money = 1;
            int team_score = 4270;
            int team_zonesScore = 26;
            string achievement_name = "area20";
            string visibleVertex_name = "vertex19";
            string visibleVertex_team = "none";
            string visibleEdge_node1 = "vertex0";
            string visibleEdge_node2 = "vertex11";
            string visibleEntity_status = "normal";
            int probedVertex_value = 4;
            int surveyedEdge_weight = 2;
            int inspectedEntity_maxHealth = 9;

            writer.Write("<message timestamp=\"" + timestamp + "\" type=\"request-action\">"
                            + "<perception deadline=\"" + perception_deadline + "\" id=\"" + perception_id + "\">"
                            + "<simulation step=\"" + simulation_step + "\"/>"
                            + "<self energy=\"" + self_energy + "\" health=\"" + self_health + "\" lastAction=\"" + self_lastAction + "\" "
                            + "lastActionParam=\"" + self_lastActionParam + "\" lastActionResult=\"" + self_lastActionResult + "\" maxEnergy=\"" + self_maxEnergy + "\" "
                            + "maxEnergyDisabled=\"" + self_maxEnergyDisabled + "\" maxHealth=\"" + self_maxHealth + "\" position=\"" + self_position + "\" "
                            + "strength=\"" + self_strength + "\" visRange=\"" + self_visRange + "\" zoneScore=\"" + self_zoneScore + "\"/>"
                            + "<team lastStepScore=\"" + team_lastStepScore + "\" money=\"" + team_money + "\" score=\"" + team_score + "\" zonesScore=\"" + team_zonesScore + "\"/>"
                            
                            + "<visibleVertices>"
                            + "<visibleVertex name=\"" + visibleVertex_name + "\" team=\"" + visibleVertex_team + "\"/>"
                            + "</visibleVertices>"
                            + "<visibleEdges>"
                            + "<visibleEdge node1=\"" + visibleEdge_node1 + "\" node2=\"" + visibleEdge_node2 + "\"/>"
                            + "</visibleEdges>"
                            + "<visibleEntities>"
                            + "<visibleEntity name=\"b5\" team=\"B\" node=\"vertex0\" "
                            + "status=\"" + visibleEntity_status + "\"/>"
                            + "</visibleEntities>"
                            + "<probedVertices>"
                            + "<probedVertex name=\"vertex18\" value=\"" + probedVertex_value + "\"/>"
                            + "</probedVertices>"
                            + "<surveyedEdges>"
                            + "<surveyedEdge node1=\"vertex3\" node2=\"vertex7\" weight=\"" + surveyedEdge_weight + "\"/>"
                            + "</surveyedEdges>"
                            + "<inspectedEntities>"
                            + "<inspectedEntity energy=\"8\" health=\"9\" maxEnergy=\"8\" "
                            + "maxHealth=\"" + inspectedEntity_maxHealth + "\" name=\"b5\" node=\"vertex10\" role=\"role2\" "
                            + "strength=\"6\" team=\"B\" visRange=\"2\"/>"
                            + "</inspectedEntities>"
                            + "</perception>"
                            + "</message>");

            writer.Flush();
            writer.BaseStream.WriteByte(0);
            writer.BaseStream.Flush();

            memStream.Position = 0;

            reader = new StreamReader(memStream);
            servCom = new ServerCommunication(reader, writer);

            ReceiveMessage packet = (ReceiveMessage)servCom.DeserializePacket();
            InternalReceiveMessage message = packet.Message;

            RequestActionMessage requestActionData = (RequestActionMessage)message;
            PerceptionMessage perceptionData = (PerceptionMessage)requestActionData.Response;
            SimulationMessage simulationData = (SimulationMessage)perceptionData.Elements[0];
            SelfMessage selfData = (SelfMessage)perceptionData.Elements[1];
            TeamMessage teamData = (TeamMessage)perceptionData.Elements[2];
            VisibleVertexMessage visibleVertexData = (VisibleVertexMessage)((VisibleVerticesMessage)perceptionData.Elements[3]).VisibleVertices[0];
            VisibleEdgeMessage visibleEdgeData = (VisibleEdgeMessage)((VisibleEdgesMessage)perceptionData.Elements[4]).VisibleEdges[0];
            VisibleEntityMessage visibleEntityData = (VisibleEntityMessage)((VisibleEntitiesMessage)perceptionData.Elements[5]).VisibleEntities[0];
            ProbedVertexMessage probedVertexData = (ProbedVertexMessage)((ProbedVerticesMessage)perceptionData.Elements[6]).ProbedVertices[0];
            SurveyedEdgeMessage surveyedEdgeData = (SurveyedEdgeMessage)((SurveyedEdgesMessage)perceptionData.Elements[7]).SurveyedEdges[0];
            InspectedEntityMessage inspectedEntityData = (InspectedEntityMessage)((InspectedEntitiesMessage)perceptionData.Elements[8]).InspectedEntities[0];

            Assert.AreEqual("perception", perceptionData.MessageName);
            Assert.AreEqual("simulation", simulationData.MessageName);
            Assert.AreEqual("self", selfData.MessageName);
            Assert.AreEqual("team", teamData.MessageName);

            Assert.AreEqual(timestamp, packet.Timestamp);
            Assert.AreEqual(perception_deadline, perceptionData.Deadline);
            Assert.AreEqual(perception_id, perceptionData.Id);
            Assert.AreEqual(simulation_step, simulationData.Step);
            Assert.AreEqual(self_energy, selfData.Energy);
            Assert.AreEqual(self_lastActionParam, selfData.LastActionParam);
            Assert.AreEqual(team_lastStepScore, teamData.LastStepScore);
            Assert.AreEqual(visibleVertex_name, visibleVertexData.Name);
            Assert.AreEqual(visibleEdge_node1, visibleEdgeData.Node1);
            Assert.AreEqual(visibleEntity_status, visibleEntityData.Status);
            Assert.AreEqual(probedVertex_value, probedVertexData.Value);
            Assert.AreEqual(surveyedEdge_weight, surveyedEdgeData.Weight);
            Assert.AreEqual(inspectedEntity_maxHealth, inspectedEntityData.MaxHealth);

        }

        [Test]
        public void ReceiveMessageRequestActionNoAchievementsAlternative_Connected_XmlMessageReceived()
        {
            long timestamp = 1297263230578;
            long perception_deadline = 1297263232578;
            int perception_id = 201;
            int simulation_step = 200;
            int self_energy = 19;
            int self_health = 9;
            string self_lastAction = "skip";
            string self_lastActionParam = "";
            string self_lastActionResult = "successful";
            int self_maxEnergy = 19;
            int self_maxEnergyDisabled = 9;
            int self_maxHealth = 9;
            string self_position = "vertex4";
            int self_strength = 5;
            int self_visRange = 5;
            int self_zoneScore = 27;
            int team_lastStepScore = 27;
            int team_money = 1;
            int team_score = 4270;
            int team_zonesScore = 26;
            string achievement_name = "area20";
            string visibleVertex_name = "vertex19";
            string visibleVertex_team = "none";
            string visibleEdge_node1 = "vertex0";
            string visibleEdge_node2 = "vertex11";
            string visibleEntity_status = "normal";
            int probedVertex_value = 4;
            int surveyedEdge_weight = 2;
            int inspectedEntity_maxHealth = 9;

            writer.Write("<message timestamp=\"" + timestamp + "\" type=\"request-action\">"
                            + "<perception deadline=\"" + perception_deadline + "\" id=\"" + perception_id + "\">"
                            + "<simulation step=\"" + simulation_step + "\"/>"
                            + "<self energy=\"" + self_energy + "\" health=\"" + self_health + "\" lastAction=\"" + self_lastAction + "\" "
                            + "lastActionParam=\"" + self_lastActionParam + "\" lastActionResult=\"" + self_lastActionResult + "\" maxEnergy=\"" + self_maxEnergy + "\" "
                            + "maxEnergyDisabled=\"" + self_maxEnergyDisabled + "\" maxHealth=\"" + self_maxHealth + "\" position=\"" + self_position + "\" "
                            + "strength=\"" + self_strength + "\" visRange=\"" + self_visRange + "\" zoneScore=\"" + self_zoneScore + "\"/>"
                            + "<team lastStepScore=\"" + team_lastStepScore + "\" money=\"" + team_money + "\" score=\"" + team_score + "\" zonesScore=\"" + team_zonesScore + "\">"
                            + "<achievements>"
                            + "</achievements>"
                            + "</team>"
                            + "<visibleVertices>"
                            + "<visibleVertex name=\"" + visibleVertex_name + "\" team=\"" + visibleVertex_team + "\"/>"
                            + "</visibleVertices>"
                            + "<visibleEdges>"
                            + "<visibleEdge node1=\"" + visibleEdge_node1 + "\" node2=\"" + visibleEdge_node2 + "\"/>"
                            + "</visibleEdges>"
                            + "<visibleEntities>"
                            + "<visibleEntity name=\"b5\" team=\"B\" node=\"vertex0\" "
                            + "status=\"" + visibleEntity_status + "\"/>"
                            + "</visibleEntities>"
                            + "<probedVertices>"
                            + "<probedVertex name=\"vertex18\" value=\"" + probedVertex_value + "\"/>"
                            + "</probedVertices>"
                            + "<surveyedEdges>"
                            + "<surveyedEdge node1=\"vertex3\" node2=\"vertex7\" weight=\"" + surveyedEdge_weight + "\"/>"
                            + "</surveyedEdges>"
                            + "<inspectedEntities>"
                            + "<inspectedEntity energy=\"8\" health=\"9\" maxEnergy=\"8\" "
                            + "maxHealth=\"" + inspectedEntity_maxHealth + "\" name=\"b5\" node=\"vertex10\" role=\"role2\" "
                            + "strength=\"6\" team=\"B\" visRange=\"2\"/>"
                            + "</inspectedEntities>"
                            + "</perception>"
                            + "</message>");

            writer.Flush();
            writer.BaseStream.WriteByte(0);
            writer.BaseStream.Flush();

            memStream.Position = 0;

            reader = new StreamReader(memStream);
            servCom = new ServerCommunication(reader, writer);

            ReceiveMessage packet = (ReceiveMessage)servCom.DeserializePacket();
            InternalReceiveMessage message = packet.Message;

            RequestActionMessage requestActionData = (RequestActionMessage)message;
            PerceptionMessage perceptionData = (PerceptionMessage)requestActionData.Response;
            SimulationMessage simulationData = (SimulationMessage)perceptionData.Elements[0];
            SelfMessage selfData = (SelfMessage)perceptionData.Elements[1];
            TeamMessage teamData = (TeamMessage)perceptionData.Elements[2];
            VisibleVertexMessage visibleVertexData = (VisibleVertexMessage)((VisibleVerticesMessage)perceptionData.Elements[3]).VisibleVertices[0];
            VisibleEdgeMessage visibleEdgeData = (VisibleEdgeMessage)((VisibleEdgesMessage)perceptionData.Elements[4]).VisibleEdges[0];
            VisibleEntityMessage visibleEntityData = (VisibleEntityMessage)((VisibleEntitiesMessage)perceptionData.Elements[5]).VisibleEntities[0];
            ProbedVertexMessage probedVertexData = (ProbedVertexMessage)((ProbedVerticesMessage)perceptionData.Elements[6]).ProbedVertices[0];
            SurveyedEdgeMessage surveyedEdgeData = (SurveyedEdgeMessage)((SurveyedEdgesMessage)perceptionData.Elements[7]).SurveyedEdges[0];
            InspectedEntityMessage inspectedEntityData = (InspectedEntityMessage)((InspectedEntitiesMessage)perceptionData.Elements[8]).InspectedEntities[0];

            Assert.AreEqual("perception", perceptionData.MessageName);
            Assert.AreEqual("simulation", simulationData.MessageName);
            Assert.AreEqual("self", selfData.MessageName);
            Assert.AreEqual("team", teamData.MessageName);

            Assert.AreEqual(timestamp, packet.Timestamp);
            Assert.AreEqual(perception_deadline, perceptionData.Deadline);
            Assert.AreEqual(perception_id, perceptionData.Id);
            Assert.AreEqual(simulation_step, simulationData.Step);
            Assert.AreEqual(self_energy, selfData.Energy);
            Assert.AreEqual(self_lastActionParam, selfData.LastActionParam);
            Assert.AreEqual(team_lastStepScore, teamData.LastStepScore);
            Assert.AreEqual(visibleVertex_name, visibleVertexData.Name);
            Assert.AreEqual(visibleEdge_node1, visibleEdgeData.Node1);
            Assert.AreEqual(visibleEntity_status, visibleEntityData.Status);
            Assert.AreEqual(probedVertex_value, probedVertexData.Value);
            Assert.AreEqual(surveyedEdge_weight, surveyedEdgeData.Weight);
            Assert.AreEqual(inspectedEntity_maxHealth, inspectedEntityData.MaxHealth);

        }

        [Test]
        public void ReceiveMessageRequestActionNoAchievementsAlternative2_Connected_XmlMessageReceived()
        {
            long timestamp = 1297263230578;
            long perception_deadline = 1297263232578;
            int perception_id = 201;
            int simulation_step = 200;
            int self_energy = 19;
            int self_health = 9;
            string self_lastAction = "skip";
            string self_lastActionParam = "";
            string self_lastActionResult = "successful";
            int self_maxEnergy = 19;
            int self_maxEnergyDisabled = 9;
            int self_maxHealth = 9;
            string self_position = "vertex4";
            int self_strength = 5;
            int self_visRange = 5;
            int self_zoneScore = 27;
            int team_lastStepScore = 27;
            int team_money = 1;
            int team_score = 4270;
            int team_zonesScore = 26;
            string achievement_name = "area20";
            string visibleVertex_name = "vertex19";
            string visibleVertex_team = "none";
            string visibleEdge_node1 = "vertex0";
            string visibleEdge_node2 = "vertex11";
            string visibleEntity_status = "normal";
            int probedVertex_value = 4;
            int surveyedEdge_weight = 2;
            int inspectedEntity_maxHealth = 9;

            writer.Write("<message timestamp=\"" + timestamp + "\" type=\"request-action\">"
                            + "<perception deadline=\"" + perception_deadline + "\" id=\"" + perception_id + "\">"
                            + "<simulation step=\"" + simulation_step + "\"/>"
                            + "<self energy=\"" + self_energy + "\" health=\"" + self_health + "\" lastAction=\"" + self_lastAction + "\" "
                            + "lastActionParam=\"" + self_lastActionParam + "\" lastActionResult=\"" + self_lastActionResult + "\" maxEnergy=\"" + self_maxEnergy + "\" "
                            + "maxEnergyDisabled=\"" + self_maxEnergyDisabled + "\" maxHealth=\"" + self_maxHealth + "\" position=\"" + self_position + "\" "
                            + "strength=\"" + self_strength + "\" visRange=\"" + self_visRange + "\" zoneScore=\"" + self_zoneScore + "\"/>"
                            + "<team lastStepScore=\"" + team_lastStepScore + "\" money=\"" + team_money + "\" score=\"" + team_score + "\" zonesScore=\"" + team_zonesScore + "\">"
                            + "<achievements/>"
                            + "</team>"
                            + "<visibleVertices>"
                            + "<visibleVertex name=\"" + visibleVertex_name + "\" team=\"" + visibleVertex_team + "\"/>"
                            + "</visibleVertices>"
                            + "<visibleEdges>"
                            + "<visibleEdge node1=\"" + visibleEdge_node1 + "\" node2=\"" + visibleEdge_node2 + "\"/>"
                            + "</visibleEdges>"
                            + "<visibleEntities>"
                            + "<visibleEntity name=\"b5\" team=\"B\" node=\"vertex0\" "
                            + "status=\"" + visibleEntity_status + "\"/>"
                            + "</visibleEntities>"
                            + "<probedVertices>"
                            + "<probedVertex name=\"vertex18\" value=\"" + probedVertex_value + "\"/>"
                            + "</probedVertices>"
                            + "<surveyedEdges>"
                            + "<surveyedEdge node1=\"vertex3\" node2=\"vertex7\" weight=\"" + surveyedEdge_weight + "\"/>"
                            + "</surveyedEdges>"
                            + "<inspectedEntities>"
                            + "<inspectedEntity energy=\"8\" health=\"9\" maxEnergy=\"8\" "
                            + "maxHealth=\"" + inspectedEntity_maxHealth + "\" name=\"b5\" node=\"vertex10\" role=\"role2\" "
                            + "strength=\"6\" team=\"B\" visRange=\"2\"/>"
                            + "</inspectedEntities>"
                            + "</perception>"
                            + "</message>");

            writer.Flush();
            writer.BaseStream.WriteByte(0);
            writer.BaseStream.Flush();

            memStream.Position = 0;

            reader = new StreamReader(memStream);
            servCom = new ServerCommunication(reader, writer);

            ReceiveMessage packet = (ReceiveMessage)servCom.DeserializePacket();
            InternalReceiveMessage message = packet.Message;

            RequestActionMessage requestActionData = (RequestActionMessage)message;
            PerceptionMessage perceptionData = (PerceptionMessage)requestActionData.Response;
            SimulationMessage simulationData = (SimulationMessage)perceptionData.Elements[0];
            SelfMessage selfData = (SelfMessage)perceptionData.Elements[1];
            TeamMessage teamData = (TeamMessage)perceptionData.Elements[2];
            VisibleVertexMessage visibleVertexData = (VisibleVertexMessage)((VisibleVerticesMessage)perceptionData.Elements[3]).VisibleVertices[0];
            VisibleEdgeMessage visibleEdgeData = (VisibleEdgeMessage)((VisibleEdgesMessage)perceptionData.Elements[4]).VisibleEdges[0];
            VisibleEntityMessage visibleEntityData = (VisibleEntityMessage)((VisibleEntitiesMessage)perceptionData.Elements[5]).VisibleEntities[0];
            ProbedVertexMessage probedVertexData = (ProbedVertexMessage)((ProbedVerticesMessage)perceptionData.Elements[6]).ProbedVertices[0];
            SurveyedEdgeMessage surveyedEdgeData = (SurveyedEdgeMessage)((SurveyedEdgesMessage)perceptionData.Elements[7]).SurveyedEdges[0];
            InspectedEntityMessage inspectedEntityData = (InspectedEntityMessage)((InspectedEntitiesMessage)perceptionData.Elements[8]).InspectedEntities[0];

            Assert.AreEqual("perception", perceptionData.MessageName);
            Assert.AreEqual("simulation", simulationData.MessageName);
            Assert.AreEqual("self", selfData.MessageName);
            Assert.AreEqual("team", teamData.MessageName);

            Assert.AreEqual(timestamp, packet.Timestamp);
            Assert.AreEqual(perception_deadline, perceptionData.Deadline);
            Assert.AreEqual(perception_id, perceptionData.Id);
            Assert.AreEqual(simulation_step, simulationData.Step);
            Assert.AreEqual(self_energy, selfData.Energy);
            Assert.AreEqual(self_lastActionParam, selfData.LastActionParam);
            Assert.AreEqual(team_lastStepScore, teamData.LastStepScore);
            Assert.AreEqual(visibleVertex_name, visibleVertexData.Name);
            Assert.AreEqual(visibleEdge_node1, visibleEdgeData.Node1);
            Assert.AreEqual(visibleEntity_status, visibleEntityData.Status);
            Assert.AreEqual(probedVertex_value, probedVertexData.Value);
            Assert.AreEqual(surveyedEdge_weight, surveyedEdgeData.Weight);
            Assert.AreEqual(inspectedEntity_maxHealth, inspectedEntityData.MaxHealth);

        }

        [Test]
        public void ReceiveMessageRequestActionAlternateXmlDoc_Connected_XmlMessageReceived()
        {
            long timestamp = 1297263230578;
            long perception_deadline = 1297263232578;
            int perception_id = 201;
            int simulation_step = 200;
            int self_energy = 19;
            int self_health = 9;
            string self_lastAction = "skip";
            string self_lastActionParam = "";
            string self_lastActionResult = "successful";
            int self_maxEnergy = 19;
            int self_maxEnergyDisabled = 9;
            int self_maxHealth = 9;
            string self_position = "vertex4";
            int self_strength = 5;
            int self_visRange = 5;
            int self_zoneScore = 27;
            int team_lastStepScore = 27;
            int team_money = 1;
            int team_score = 4270;
            int team_zonesScore = 26;
            string achievement_name = "area20";
            string visibleVertex_name = "vertex19";
            string visibleVertex_team = "none";
            string visibleEdge_node1 = "vertex0";
            string visibleEdge_node2 = "vertex11";
            string visibleEntity_status = "normal";
            int probedVertex_value = 4;
            int surveyedEdge_weight = 2;
            int inspectedEntity_maxHealth = 9;

            writer.Write("<message timestamp=\"" + timestamp + "\" type=\"request-action\">"
                            + "<perception deadline=\"" + perception_deadline + "\" id=\"" + perception_id + "\">"
                            + "<simulation step=\"" + simulation_step + "\"/>"
                            + "<self energy=\"" + self_energy + "\" health=\"" + self_health + "\" lastAction=\"" + self_lastAction + "\" "
                            + "lastActionParam=\"" + self_lastActionParam + "\" lastActionResult=\"" + self_lastActionResult + "\" maxEnergy=\"" + self_maxEnergy + "\" "
                            + "maxEnergyDisabled=\"" + self_maxEnergyDisabled + "\" maxHealth=\"" + self_maxHealth + "\" position=\"" + self_position + "\" "
                            + "strength=\"" + self_strength + "\" visRange=\"" + self_visRange + "\" zoneScore=\"" + self_zoneScore + "\"/>"
                            + "<team lastStepScore=\"" + team_lastStepScore + "\" money=\"" + team_money + "\" score=\"" + team_score + "\" zonesScore=\"" + team_zonesScore + "\"/>"

                            + "<visibleVertices>"
                            + "</visibleVertices>"
                            + "<visibleEntities>"
                            + "<visibleEntity name=\"b5\" team=\"B\" node=\"vertex0\" "
                            + "status=\"" + visibleEntity_status + "\"/>"
                            + "</visibleEntities>"
                            + "<probedVertices/>"
                            + "<surveyedEdges>"
                            + "<surveyedEdge node1=\"vertex3\" node2=\"vertex7\" weight=\"" + surveyedEdge_weight + "\"/>"
                            + "</surveyedEdges>"
                            + "<inspectedEntities>"
                            + "<inspectedEntity energy=\"8\" health=\"9\" maxEnergy=\"8\" "
                            + "maxHealth=\"" + inspectedEntity_maxHealth + "\" name=\"b5\" node=\"vertex10\" role=\"role2\" "
                            + "strength=\"6\" team=\"B\" visRange=\"2\"/>"
                            + "</inspectedEntities>"
                            + "</perception>"
                            + "</message>");

            writer.Flush();
            writer.BaseStream.WriteByte(0);
            writer.BaseStream.Flush();

            memStream.Position = 0;

            reader = new StreamReader(memStream);
            servCom = new ServerCommunication(reader, writer);

            ReceiveMessage packet = (ReceiveMessage)servCom.DeserializePacket();
            InternalReceiveMessage message = packet.Message;

            //RequestActionMessage requestActionData = (RequestActionMessage)message;
            //PerceptionMessage perceptionData = (PerceptionMessage)requestActionData.Response;
            //SimulationMessage simulationData = (SimulationMessage)perceptionData.Elements[0];
            //SelfMessage selfData = (SelfMessage)perceptionData.Elements[1];
            //TeamMessage teamData = (TeamMessage)perceptionData.Elements[2];
            //VisibleVertexMessage visibleVertexData = (VisibleVertexMessage)((VisibleVerticesMessage)perceptionData.Elements[3]).VisibleVertices[0];
            //VisibleEdgeMessage visibleEdgeData = (VisibleEdgeMessage)((VisibleEdgesMessage)perceptionData.Elements[4]).VisibleEdges[0];
            //VisibleEntityMessage visibleEntityData = (VisibleEntityMessage)((VisibleEntitiesMessage)perceptionData.Elements[5]).VisibleEntities[0];
            //ProbedVertexMessage probedVertexData = (ProbedVertexMessage)((ProbedVerticesMessage)perceptionData.Elements[6]).ProbedVertices[0];
            //SurveyedEdgeMessage surveyedEdgeData = (SurveyedEdgeMessage)((SurveyedEdgesMessage)perceptionData.Elements[7]).SurveyedEdges[0];
            //InspectedEntityMessage inspectedEntityData = (InspectedEntityMessage)((InspectedEntitiesMessage)perceptionData.Elements[8]).InspectedEntities[0];

            //Assert.AreEqual("perception", perceptionData.MessageName);
            //Assert.AreEqual("simulation", simulationData.MessageName);
            //Assert.AreEqual("self", selfData.MessageName);
            //Assert.AreEqual("team", teamData.MessageName);

            //Assert.AreEqual(timestamp, packet.Timestamp);
            //Assert.AreEqual(perception_deadline, perceptionData.Deadline);
            //Assert.AreEqual(perception_id, perceptionData.Id);
            //Assert.AreEqual(simulation_step, simulationData.Step);
            //Assert.AreEqual(self_energy, selfData.Energy);
            //Assert.AreEqual(self_lastActionParam, selfData.LastActionParam);
            //Assert.AreEqual(team_lastStepScore, teamData.LastStepScore);
            //Assert.AreEqual(visibleVertex_name, visibleVertexData.Name);
            //Assert.AreEqual(visibleEdge_node1, visibleEdgeData.Node1);
            //Assert.AreEqual(visibleEntity_status, visibleEntityData.Status);
            //Assert.AreEqual(probedVertex_value, probedVertexData.Value);
            //Assert.AreEqual(surveyedEdge_weight, surveyedEdgeData.Weight);
            //Assert.AreEqual(inspectedEntity_maxHealth, inspectedEntityData.MaxHealth);
            Assert.Pass();
        }

     
    }
    
}
