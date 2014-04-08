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
	public class ConvertSimStartToIilPerceptCollection : JSConverterToForeign<ReceiveMessage,IilPerceptCollection>
	{
		public override object KnownID
		{
			get
			{
				return typeof(SimStartMessage);
			}
		}

		public override IilPerceptCollection BeginConversionToForeign(ReceiveMessage gobj)
		{
			var simStart = (SimStartMessage)gobj.Message;
			return new IilPerceptCollection(
				new IilPercept	(	"simStart"
								,	new IilFunction("id", new IilNumeral(simStart.Id))
								,	new IilFunction("steps", new IilNumeral(simStart.Steps))
								,	new IilFunction("edges", new IilNumeral(simStart.Edges))
								,	new IilFunction("vertices", new IilNumeral(simStart.Vertices))
								,	new IilFunction("role", new IilIdentifier(simStart.Role)))
								);

		}
	}
}
