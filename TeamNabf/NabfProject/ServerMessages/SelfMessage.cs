using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class SelfMessage : InternalReceiveMessage
    {
        private int energy;

        public int Energy
        {
            get { return energy; }
        }
        private int health;

        public int Health
        {
            get { return health; }
        }
        private string lastAction;

        public string LastAction
        {
            get { return lastAction; }
        }
        private string lastActionParam;

        public string LastActionParam
        {
            get { return lastActionParam; }
        }
        private string lastActionResult;

        public string LastActionResult
        {
            get { return lastActionResult; }
        }
        private int maxEnergy;

        public int MaxEnergy
        {
            get { return maxEnergy; }
        }
        private int maxEnergyDisabled;

        public int MaxEnergyDisabled
        {
            get { return maxEnergyDisabled; }
        }
        private int maxHealth;

        public int MaxHealth
        {
            get { return maxHealth; }
        }
        private string position;

        public string Position
        {
            get { return position; }
        }
        private int strength;

        public int Strength
        {
            get { return strength; }
        }
        private int visRange;

        public int VisRange
        {
            get { return visRange; }
        }
        private int zoneScore;

        public int ZoneScore
        {
            get { return zoneScore; }
        }

        private string messageName;

        public string MessageName
        {
            get { return messageName; }
        }
        
        public override void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();

            messageName = reader.LocalName;
            energy = Convert.ToInt32(reader["energy"]);
            health = Convert.ToInt32(reader["health"]);
            lastAction = reader["lastAction"];
            lastActionParam = reader["lastActionParam"];
            lastActionResult = reader["lastActionResult"];
            maxEnergy = Convert.ToInt32(reader["maxEnergy"]);
            maxEnergyDisabled = Convert.ToInt32(reader["maxEnergyDisabled"]);
            maxHealth = Convert.ToInt32(reader["maxHealth"]);
            position = reader["position"];
            strength = Convert.ToInt32(reader["strength"]);
            visRange = Convert.ToInt32(reader["visRange"]);
            zoneScore = Convert.ToInt32(reader["zoneScore"]);

            if (reader.IsEmptyElement)
            {
                reader.Read();
                reader.MoveToContent();
            }
        }
    }
}
