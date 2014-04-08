
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class ServerMessageFactory
    {
        private static ServerMessageFactory instance = new ServerMessageFactory();
        public static ServerMessageFactory Instance
        {
            get{ return instance;}
        }

        public InternalReceiveMessage ConstructMessage(string type)
        {
            if ("auth-response".Equals(type))
                return new AuthenticationResponseMessage();
            if ("sim-start".Equals(type))
                return new SimStartMessage();
            if ("sim-end".Equals(type))
                return new SimEndMessage();
            if ("bye".Equals(type))
                return new ByeMessage();
            if ("request-action".Equals(type))
                return new RequestActionMessage();
            if ("perception".Equals(type))
                return new PerceptionMessage();
            if ("self".Equals(type))
                return new SelfMessage();
            if ("simulation".Equals(type))
                return new SimulationMessage();
            if ("team".Equals(type))
                return new TeamMessage();
            if ("achievements".Equals(type))
                return new AchievementsMessage();
            if ("achievement".Equals(type))
                return new AchievementMessage();
            if ("visibleVertices".Equals(type))
                return new VisibleVerticesMessage();
            if ("visibleVertex".Equals(type))
                return new VisibleVertexMessage();
            if ("visibleEdges".Equals(type))
                return new VisibleEdgesMessage();
            if ("visibleEdge".Equals(type))
                return new VisibleEdgeMessage();
            if ("visibleEntities".Equals(type))
                return new VisibleEntitiesMessage();
            if ("visibleEntity".Equals(type))
                return new VisibleEntityMessage();
            if ("probedVertices".Equals(type))
                return new ProbedVerticesMessage();
            if ("probedVertex".Equals(type))
                return new ProbedVertexMessage();
            if ("surveyedEdges".Equals(type))
                return new SurveyedEdgesMessage();
            if ("surveyedEdge".Equals(type))
                return new SurveyedEdgeMessage();
            if ("inspectedEntities".Equals(type))
                return new InspectedEntitiesMessage();
            if ("inspectedEntity".Equals(type))
                return new InspectedEntityMessage();
            return null;
        }
    }
}
