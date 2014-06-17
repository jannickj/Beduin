using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using JSLibrary.Conversion;
using JSLibrary.IiLang;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;
using NabfProject.NoticeBoardModel;

namespace NabfProject.Parsers.NoticeConverters
{
    public class NoticeConverter : JSConverter<Notice, IilElement>
    {
        public override object ForeignID
        {
            get
            {
                return "notice";
            }
        }

        public override IilElement BeginConversionToForeign(Notice gobj)
        {
            string name = gobj.GetType().Name;
            int jobType = (int)NoticeBoardHelpers.NoticeToJobType(gobj);

            IilFunction optional = null;

            if (gobj is RepairJob)
                optional = new IilFunction("agentToRepair", new IilIdentifier(((RepairJob)gobj).AgentToRepair));
            else if (gobj is OccupyJob)
                optional = new IilFunction("zoneNodes", ((OccupyJob)gobj).ZoneNodes.Select(n => (new IilIdentifier(n.Name))));
            else if (gobj is AttackJob)
                optional = new IilFunction("timeStamp", new IilNumeral(((AttackJob)gobj).TimeStamp));

            IilPerceptCollection ipc;

            if (optional != null)
                ipc = new IilPerceptCollection(new IilPercept("notice"
                    , new IilFunction("type", new IilNumeral(jobType))
                    , new IilFunction("agentsNeeded", new IilNumeral(gobj.AgentsNeeded))
                    , new IilFunction("id", new IilNumeral(gobj.Id))
                    , new IilFunction("value", new IilNumeral(gobj.Value))
                    , new IilFunction("whichNodes", gobj.WhichNodes.Select(n => (new IilIdentifier(n.Name))))
                    , optional
                    ));
            else
                ipc = new IilPerceptCollection(new IilPercept("notice"
                    , new IilFunction("type", new IilNumeral(jobType))
                    , new IilFunction("agentsNeeded", new IilNumeral(gobj.AgentsNeeded))
                    , new IilFunction("id", new IilNumeral(gobj.Id))
                    , new IilFunction("value", new IilNumeral(gobj.Value))
                    , new IilFunction("whichNodes", gobj.WhichNodes.Select(n => (new IilIdentifier(n.Name))))
                    ));

            return ipc;
        }

        public override Notice BeginConversionToKnown(IilElement fobj)
        {
            throw new NotImplementedException();
        }
    }
}
