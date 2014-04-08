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
	public class ConvertByeMessageToIilPerceptCollection : JSConverterToForeign<ReceiveMessage,IilPerceptCollection>
	{
		public override object KnownID
		{
			get
			{
				return typeof(ByeMessage);
			}
		}

		public override IilPerceptCollection BeginConversionToForeign(ReceiveMessage gobj)
		{
			var bye = (ByeMessage)gobj.Message;
			return new IilPerceptCollection(
				new IilPercept("bye"));

		}
	}
}
