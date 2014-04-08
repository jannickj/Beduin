using JSLibrary.Conversion;
using JSLibrary.IiLang;
using NabfProject.Events;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;
using NabfProject.AI;

namespace NabfProject.Parsers.AgentMasterToAgentConverters
{
    public class ConverterFiredFromJob : JSConverterToForeign<FiredFromJobEvent, IilPerceptCollection>
    {
        public AgentMasterDataParsers Parsers { get; set; }

		public override IilPerceptCollection BeginConversionToForeign (FiredFromJobEvent gobj)
		{
			IilPerceptCollection ipc;

			ipc = new IilPerceptCollection
				(
					new IilPercept("firedFromJob"),
					new IilPercept("noticeId", new IilNumeral(gobj.Notice.Id))
				);

			return ipc;
		}
    }
}
