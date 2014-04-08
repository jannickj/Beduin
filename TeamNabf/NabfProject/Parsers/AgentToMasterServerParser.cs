using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.Conversion;
using JSLibrary.IiLang;
using JSLibrary.IiLang.DataContainers;
using NabfProject.Parsers.AgentToMarsConverters;
using NabfProject.ServerMessages;

namespace NabfProject.Parsers
{
	public class AgentToMasterServerParser : JSConversionTool<object,IilElement>
	{
        public AgentToMasterServerParser()
		{
			this.IdOfForeign = new JSConversionIDFetcherSimple<IilElement>(fetchId);
		}

		private object fetchId(IilElement element)
		{
            var ipc = (IilAction)element;

            return ipc.Name;
		}

	}
}
