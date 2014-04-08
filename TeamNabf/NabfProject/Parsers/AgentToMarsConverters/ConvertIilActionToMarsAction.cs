using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.Conversion;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;
using NabfProject.ServerMessages;

namespace NabfProject.Parsers.AgentToMarsConverters
{
	public class ConvertIilActionToMarsAction : JSConverterToForeign<IilAction, ActionMessage>
	{

		public override ActionMessage BeginConversionToForeign(IilAction gobj)
		{
			
			
			string acttype = gobj.Name;
			var vals = gobj.Parameters;
			var id = (int)((IilNumeral)vals[0]).Value;
			ActionMessage actmsg;
			if(vals.Count == 1)
				actmsg =new ActionMessage(id, acttype);
			else
			{
				var param = ((IilIdentifier)vals[1]).Value;
				actmsg = new ActionMessage(id, acttype,param);
			} 
			return actmsg;
		}

	}
}
