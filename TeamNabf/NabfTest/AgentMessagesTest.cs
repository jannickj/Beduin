using JSLibrary.Conversion;
using JSLibrary.IiLang.DataContainers;
using JSLibrary.IiLang.Parameters;
using NabfProject.Parsers.MarsToAgentConverters;
using NabfProject.ServerMessages;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace NabfTest
{
    [TestFixture]
    public class AgentMessagesTest
    {
        [Test]
        public void Conversion_SimulationMessage_CorrectiilPercept()
        {
            string fieldName = "step";
            int fieldValue = 42;
            string messageName = "messageName";
            string messageValue = "testname";

            var converter = new JSConversionTool<InternalReceiveMessage, IilPercept>();

            converter.AddConverter(new ConvertSimulationToPercept());

            SimulationMessage simMes = new SimulationMessage();
            SetPrivateField(simMes, fieldName, fieldValue);
            SetPrivateField(simMes, messageName, messageValue);
            var percept = converter.ConvertToForeign(simMes);

            var param = (IilNumeral) ((IilFunction) percept.Parameters[0]).Parameters[0];

            Assert.AreEqual(fieldValue, param.Value);
            Assert.AreEqual(messageValue, percept.Name);
        }

        private static void SetPrivateField(object obj, string name, object value)
        {
            var field = obj.GetType().GetField(name, BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.GetField);
            field.SetValue(obj, value);
        }
    }
}
