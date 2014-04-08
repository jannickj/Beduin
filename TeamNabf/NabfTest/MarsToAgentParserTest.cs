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
using NabfProject.Parsers;
using JSLibrary.IiLang;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;

namespace NabfTest
{
    [TestFixture]
	public class MarsToAgentParserTest
    { 
        ServerCommunication serverCom;
        MemoryStream memoryStream;
        StreamReader sReader;
        StreamWriter sWriter;

        [SetUp]
        public void Init()
        {
            memoryStream = new MemoryStream();
            memoryStream.Position = 0;
            sReader = new StreamReader(memoryStream);
            sWriter = new StreamWriter(memoryStream);
            serverCom = new ServerCommunication(sReader, sWriter);
        }

        
        [Test]
        public void ParseRequestAction_FullActionRequest_Success()
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
            string visibleEntity_name = "repairdude32";
            string visibleEntity_team = "b5";
            string visibleEntity_node = "vertex99";
            string visibleEntity_status = "normal";
            int probedVertex_value = 4;
            string probedVertex_name = "theplacetobe";
            string surveyedEdge_node1 = "rock";
            string surveyedEdge_node2 = "hardplace";
            int surveyedEdge_weight = 1337;
            int inspectedEntity_energy = -1;
            int inspectedEntity_health = -2;
            int inspectedEntity_maxEnergy = -3;
            int inspectedEntity_maxHealth = -4;
            string inspectedEntity_name = "nosuchagentexception";
            string inspectedEntity_node = "404nodenotfound";
            string inspectedEntity_role = "nullpointerexception";
            int inspectedEntity_strength = -5;
            string inspectedEntity_team = "arguementexception";
            int inspectedEntity_visRange = -6;

            sWriter.Write("<message timestamp=\"" + timestamp + "\" type=\"request-action\">"
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
                            + "<visibleEntity name=\"" + visibleEntity_name + "\" team=\"" + visibleEntity_team + "\" node=\"" + visibleEntity_node + "\" "
                            + "status=\""+visibleEntity_status+"\"/>"
                            + "</visibleEntities>"
                            + "<probedVertices>"
                            + "<probedVertex name=\"" + probedVertex_name + "\" value=\"" + probedVertex_value + "\"/>"
                            + "</probedVertices>"
                            + "<surveyedEdges>"
                            + "<surveyedEdge node1=\"" + surveyedEdge_node1 + "\" node2=\"" + surveyedEdge_node2 + "\" weight=\"" + surveyedEdge_weight + "\"/>"
                            + "</surveyedEdges>"
                            + "<inspectedEntities>"
                            + "<inspectedEntity energy=\"" + inspectedEntity_energy + "\" health=\"" + inspectedEntity_health + "\" maxEnergy=\"" + inspectedEntity_maxEnergy + "\" "
                            + "maxHealth=\"" + inspectedEntity_maxHealth + "\" name=\"" + inspectedEntity_name + "\" node=\"" + inspectedEntity_node + "\" role=\"" + inspectedEntity_role + "\" "
                            + "strength=\"" + inspectedEntity_strength + "\" team=\"" + inspectedEntity_team + "\" visRange=\"" + inspectedEntity_visRange + "\"/>"
                            + "</inspectedEntities>"
                            + "</perception>"
                            + "</message>");

            sWriter.Flush();
            sWriter.BaseStream.WriteByte(0);
            sWriter.BaseStream.Flush();

            memoryStream.Position = 0;

            sReader = new StreamReader(memoryStream);
            serverCom = new ServerCommunication(sReader, sWriter);

            ReceiveMessage package = (ReceiveMessage)serverCom.DeserializePacket();

            MarsToAgentParser mtap = new MarsToAgentParser();

            IilPerceptCollection ipc = (IilPerceptCollection) mtap.ConvertToForeign(package);

            Assert.AreEqual("actionRequest", ipc.Percepts[0].Name);

            Assert.AreEqual("id", ((IilFunction)ipc.Percepts[0].Parameters[0]).Name);

            Assert.AreEqual("deadline", ((IilFunction)ipc.Percepts[0].Parameters[1]).Name);

            Assert.AreEqual("timestamp", ((IilFunction)ipc.Percepts[0].Parameters[2]).Name);

            Assert.AreEqual(perception_id, ((IilNumeral)((IilFunction)ipc.Percepts[0].Parameters[0]).Parameters[0]).Value);

            Assert.AreEqual(perception_deadline, ((IilNumeral)((IilFunction)ipc.Percepts[0].Parameters[1]).Parameters[0]).Value);

            Assert.AreEqual(timestamp, ((IilNumeral)((IilFunction)ipc.Percepts[0].Parameters[2]).Parameters[0]).Value);

            Assert.AreEqual(simulation_step, ((IilNumeral)((IilFunction)ipc.Percepts[1].Parameters[0]).Parameters[0]).Value);

            Assert.AreEqual(self_energy, ((IilNumeral)((IilFunction)ipc.Percepts[2].Parameters[0]).Parameters[0]).Value);

            Assert.AreEqual(self_health, ((IilNumeral)((IilFunction)ipc.Percepts[2].Parameters[1]).Parameters[0]).Value);
            
            Assert.AreEqual(self_lastAction, ((IilIdentifier)((IilFunction)ipc.Percepts[2].Parameters[2]).Parameters[0]).Value);

            Assert.AreEqual(self_lastActionParam, ((IilIdentifier)((IilFunction)ipc.Percepts[2].Parameters[3]).Parameters[0]).Value);

            Assert.AreEqual(self_lastActionResult, ((IilIdentifier)((IilFunction)ipc.Percepts[2].Parameters[4]).Parameters[0]).Value);
            
            Assert.AreEqual(self_maxEnergy, ((IilNumeral)((IilFunction)ipc.Percepts[2].Parameters[5]).Parameters[0]).Value);
            
            Assert.AreEqual(self_maxEnergyDisabled, ((IilNumeral)((IilFunction)ipc.Percepts[2].Parameters[6]).Parameters[0]).Value);

            Assert.AreEqual(self_maxHealth, ((IilNumeral)((IilFunction)ipc.Percepts[2].Parameters[7]).Parameters[0]).Value);

            Assert.AreEqual(self_position, ((IilIdentifier)((IilFunction)ipc.Percepts[2].Parameters[8]).Parameters[0]).Value);

            Assert.AreEqual(self_strength, ((IilNumeral)((IilFunction)ipc.Percepts[2].Parameters[9]).Parameters[0]).Value);

            Assert.AreEqual(self_visRange, ((IilNumeral)((IilFunction)ipc.Percepts[2].Parameters[10]).Parameters[0]).Value);

            Assert.AreEqual(self_zoneScore, ((IilNumeral)((IilFunction)ipc.Percepts[2].Parameters[11]).Parameters[0]).Value);

            Assert.AreEqual(team_lastStepScore, ((IilNumeral)((IilFunction)ipc.Percepts[3].Parameters[0]).Parameters[0]).Value);

            Assert.AreEqual(team_money, ((IilNumeral)((IilFunction)ipc.Percepts[3].Parameters[1]).Parameters[0]).Value);

            Assert.AreEqual(team_score, ((IilNumeral)((IilFunction)ipc.Percepts[3].Parameters[2]).Parameters[0]).Value);

            Assert.AreEqual(team_zonesScore, ((IilNumeral)((IilFunction)ipc.Percepts[3].Parameters[3]).Parameters[0]).Value);

            Assert.AreEqual(achievement_name, ((IilIdentifier)((IilFunction)ipc.Percepts[3].Parameters[4]).Parameters[0]).Value);

            Assert.AreEqual(visibleVertex_name, ((IilIdentifier)((IilFunction)((IilFunction)ipc.Percepts[4].Parameters[0]).Parameters[0]).Parameters[0]).Value);

            Assert.AreEqual(visibleVertex_team, ((IilIdentifier)((IilFunction)((IilFunction)ipc.Percepts[4].Parameters[0]).Parameters[1]).Parameters[0]).Value);

            Assert.AreEqual(visibleEdge_node1, ((IilIdentifier)((IilFunction)((IilFunction)ipc.Percepts[5].Parameters[0]).Parameters[0]).Parameters[0]).Value);

            Assert.AreEqual(visibleEdge_node2, ((IilIdentifier)((IilFunction)((IilFunction)ipc.Percepts[5].Parameters[0]).Parameters[1]).Parameters[0]).Value);
                       
            Assert.AreEqual(visibleEntity_name, ((IilIdentifier)((IilFunction)((IilFunction)ipc.Percepts[6].Parameters[0]).Parameters[0]).Parameters[0]).Value);

            Assert.AreEqual(visibleEntity_team, ((IilIdentifier)((IilFunction)((IilFunction)ipc.Percepts[6].Parameters[0]).Parameters[1]).Parameters[0]).Value);

            Assert.AreEqual(visibleEntity_node, ((IilIdentifier)((IilFunction)((IilFunction)ipc.Percepts[6].Parameters[0]).Parameters[2]).Parameters[0]).Value);

            Assert.AreEqual(visibleEntity_status, ((IilIdentifier)((IilFunction)((IilFunction)ipc.Percepts[6].Parameters[0]).Parameters[3]).Parameters[0]).Value);

            Assert.AreEqual(probedVertex_name, ((IilIdentifier)((IilFunction)((IilFunction)ipc.Percepts[7].Parameters[0]).Parameters[0]).Parameters[0]).Value);

            Assert.AreEqual(probedVertex_value, ((IilNumeral)((IilFunction)((IilFunction)ipc.Percepts[7].Parameters[0]).Parameters[1]).Parameters[0]).Value);

            Assert.AreEqual(surveyedEdge_node1, ((IilIdentifier)((IilFunction)((IilFunction)ipc.Percepts[8].Parameters[0]).Parameters[0]).Parameters[0]).Value);

            Assert.AreEqual(surveyedEdge_node2, ((IilIdentifier)((IilFunction)((IilFunction)ipc.Percepts[8].Parameters[0]).Parameters[1]).Parameters[0]).Value);

            Assert.AreEqual(surveyedEdge_weight, ((IilNumeral)((IilFunction)((IilFunction)ipc.Percepts[8].Parameters[0]).Parameters[2]).Parameters[0]).Value);

            Assert.AreEqual(inspectedEntity_energy, ((IilNumeral)((IilFunction)((IilFunction)ipc.Percepts[9].Parameters[0]).Parameters[0]).Parameters[0]).Value);

            Assert.AreEqual(inspectedEntity_health, ((IilNumeral)((IilFunction)((IilFunction)ipc.Percepts[9].Parameters[0]).Parameters[1]).Parameters[0]).Value);

            Assert.AreEqual(inspectedEntity_maxEnergy, ((IilNumeral)((IilFunction)((IilFunction)ipc.Percepts[9].Parameters[0]).Parameters[2]).Parameters[0]).Value);

            Assert.AreEqual(inspectedEntity_maxHealth, ((IilNumeral)((IilFunction)((IilFunction)ipc.Percepts[9].Parameters[0]).Parameters[3]).Parameters[0]).Value);

            Assert.AreEqual(inspectedEntity_name, ((IilIdentifier)((IilFunction)((IilFunction)ipc.Percepts[9].Parameters[0]).Parameters[4]).Parameters[0]).Value);

            Assert.AreEqual(inspectedEntity_node, ((IilIdentifier)((IilFunction)((IilFunction)ipc.Percepts[9].Parameters[0]).Parameters[5]).Parameters[0]).Value);

            Assert.AreEqual(inspectedEntity_role, ((IilIdentifier)((IilFunction)((IilFunction)ipc.Percepts[9].Parameters[0]).Parameters[6]).Parameters[0]).Value);

            Assert.AreEqual(inspectedEntity_strength, ((IilNumeral)((IilFunction)((IilFunction)ipc.Percepts[9].Parameters[0]).Parameters[7]).Parameters[0]).Value);

            Assert.AreEqual(inspectedEntity_team, ((IilIdentifier)((IilFunction)((IilFunction)ipc.Percepts[9].Parameters[0]).Parameters[8]).Parameters[0]).Value);

            Assert.AreEqual(inspectedEntity_visRange, ((IilNumeral)((IilFunction)((IilFunction)ipc.Percepts[9].Parameters[0]).Parameters[9]).Parameters[0]).Value);

        }


     
    }
    
}
