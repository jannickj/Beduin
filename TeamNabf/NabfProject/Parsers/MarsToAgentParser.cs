using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.Conversion;
using JSLibrary.IiLang;
using NabfProject.ServerMessages;
using NabfProject.Parsers.MarsToAgentConverters;

namespace NabfProject.Parsers
{
	public class MarsToAgentParser : JSConversionTool<object,IilElement>
	{
		public MarsToAgentParser()
		{
			this.IdOfKnown = new JSConversionIDFetcherSimple<object>(fetchId);
            this.AddConverter(new ConvertActionRequestMessageToPerceptCollection());
            this.AddConverter(new ConvertInspectedEntitiesToPercept());
            this.AddConverter(new ConvertPerceptionMessageToIilPerceptCollection());
            this.AddConverter(new ConvertProbedVerticesToPercept());
            this.AddConverter(new ConvertSelfToPercept());
            this.AddConverter(new ConvertSimulationToPercept());
            this.AddConverter(new ConvertSurveyedEdgesToPercept());
            this.AddConverter(new ConvertTeamToPercept());
            this.AddConverter(new ConvertVisibleEdgesToPercept());
            this.AddConverter(new ConvertVisibleEntitiesToPercept());
            this.AddConverter(new ConvertVisibleVerticesToPercept());
            this.AddConverter(new ConvertSimStartToIilPerceptCollection());
            this.AddConverter(new ConvertSimEndToIilPerceptCollection());
            this.AddConverter(new ConvertByeMessageToIilPerceptCollection());
		}

		private object fetchId(object message)
		{
			if (message is ReceiveMessage)
				return ((ReceiveMessage)message).Message.GetType();
			else if (message is InternalReceiveMessage)
				return message.GetType();
			else
				throw new UnconvertableException(message);
					

			
		}
	}
}
