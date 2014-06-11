using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using NUnit.Framework;
using NabfProject.NewNoticeBoard;
using NabfProject.AI;
using System.Reflection;
using JSLibrary.Data;
using NabfProject.KnowledgeManagerModel;
using NabfProject.Events;

namespace NabfTest.NewNoticeBoardTest
{
	[TestFixture]
	public class NoticeBoardTest
	{
        NewNoticeBoard nb;
        int ID = 0;


        [SetUp]
        public void Initialization()
        {
            nb = new NewNoticeBoard();
        }

		[Test]
		public void AddInitialNotice_NoDuplicateListEmpty_Success()
		{
            Assert.Pass("test disabled");

            NewNotice n;
		}

        private object getField(object instance, bool useBase, String name)
        {
            Type t;
            if (useBase)
                t = instance.GetType().BaseType;
            else
                t = instance.GetType();

            FieldInfo f = t.GetField(name, BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.IgnoreCase);
            
            return f.GetValue(instance);
        }
        
	}
}
