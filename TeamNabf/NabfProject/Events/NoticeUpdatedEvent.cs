using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using NabfProject.NoticeBoardModel;
using XmasEngineModel.Management;

namespace NabfProject.Events
{
    public class NoticeUpdatedEvent : XmasEvent
    {
        public Int64 NoticeId {get; private set;}
        public Notice UpdatedNotice { get; private set; }

        public NoticeUpdatedEvent(Int64 noticeID, Notice updatedNotice)
        {
            NoticeId = noticeID;
            UpdatedNotice = updatedNotice;
        }

    }
}
