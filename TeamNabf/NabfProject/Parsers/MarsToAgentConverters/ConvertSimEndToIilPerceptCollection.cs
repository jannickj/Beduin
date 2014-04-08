using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.Conversion;
using JSLibrary.IiLang;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;
using NabfProject.ServerMessages;

namespace NabfProject.Parsers.MarsToAgentConverters
{
	public class ConvertSimEndToIilPerceptCollection : JSConverterToForeign<ReceiveMessage,IilPerceptCollection>
	{
		public override object KnownID
		{
			get
			{
				return typeof(SimEndMessage);
			}
		}

		public override IilPerceptCollection BeginConversionToForeign(ReceiveMessage gobj)
		{
			var simEnd = (SimEndMessage)gobj.Message;
			return new IilPerceptCollection(
				new IilPercept("simEnd",
					new IilFunction("ranking", new IilNumeral(simEnd.Ranking)),
					new IilFunction("steps", new IilNumeral(simEnd.Score))));

		}
	}
}
