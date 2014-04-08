using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.Conversion;
using JSLibrary.IiLang;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;
using NabfProject.NoticeBoardModel;
using NabfProject.Parsers.AgentToMarsConverters;
using NabfProject.ServerMessages;

namespace NabfProject.Parsers
{
	public class AgentMasterDataParsers : JSConversionTool<object,IilElement>
	{
        public AgentMasterDataParsers()
		{
			this.IdOfForeign = new JSConversionIDFetcherSimple<IilElement>(fetchId);
            this.IdOfKnown = new JSConversionIDFetcherSimple<object>(knownidfetch);
            this.AddConverter(new KnowledgeConverters.EdgeKnowledgeConverter());
            this.AddConverter(new KnowledgeConverters.NodeKnowledgeConverter());
            this.AddConverter(new KnowledgeConverters.RoleKnowledgeConverter());
            this.AddConverter(new NoticeConverters.NoticeConverter());
		}

        private object knownidfetch(object input)
        {
            if (input is Notice)
                return typeof(Notice);
            else
                return input.GetType();
        }

		private object fetchId(IilElement element)
		{
            var ipc = (IilFunction)element;

            return ipc.Name;
		}

	}
}
