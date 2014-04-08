using NabfProject.NoticeBoardModel;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using XmasEngineModel.Management;

namespace NabfProject.Events
{
    public class NewNoticeEvent : XmasEvent
    {
        public Notice Notice { get; private set; }

        public NewNoticeEvent(Notice n)
        {
            Notice = n;
			
			

        }
    }
}
