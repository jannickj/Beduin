using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NabfProject.NoticeBoardModel;
using XmasEngineModel.Management;

namespace NabfProject.Events
{
    public class NoticeRemovedEvent : XmasEvent
    {
        public Notice Notice { get; private set; }

        public NoticeRemovedEvent(Notice n)
        {
            Notice = n;
        }
    }
}
