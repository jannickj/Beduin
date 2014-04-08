using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;
using NabfProject.Parsers;
using NabfProject.ServerMessages;
using NUnit.Framework;

namespace NabfTest
{
	[TestFixture]
	public class AgentToMarsConverterTest
	{
		[Test]
		public void Convert_GotoAction_CorrectAction()
		{
			string actionType = "goto";
			string node = "v1";
			int id = 2;

			AgentToMarsParser parser = new AgentToMarsParser();

			IilAction action = new IilAction(actionType,new IilNumeral(id),new IilIdentifier(node));

			var msg = (ActionMessage) parser.ConvertToForeign(action);

			var actualType = getField(msg, false, "actionType");
			var actualId = getField(msg, false, "id");
			var actualNode = getField(msg, false, "actionParam");

			Assert.AreEqual(actionType, actionType);
			Assert.AreEqual(id, actualId);
			Assert.AreEqual(node, actualNode);

			

		}

		[Test]
		public void Convert_RechargeAction_CorrectAction()
		{

			string actionType = "recharge";
			int id = 1;

			AgentToMarsParser parser = new AgentToMarsParser();

			IilAction action = new IilAction(actionType, new IilNumeral(id));

			var msg = (ActionMessage)parser.ConvertToForeign(action);

			var actualType = getField(msg, false, "actionType");
			var actualId = getField(msg, false, "id");

			Assert.AreEqual(actionType, actionType);
			Assert.AreEqual(id, actualId);

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
