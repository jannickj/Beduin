using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace NabfProject.ServerMessages
{
    public class InspectedEntityMessage : InternalReceiveMessage
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
        private int maxEnergy;

        public int MaxEnergy
        {
            get { return maxEnergy; }
        }
        private int maxHealth;

        public int MaxHealth
        {
            get { return maxHealth; }
        }
        private string name;

        public string Name
        {
            get { return name; }
        }
        private string node;

        public string Node
        {
            get { return node; }
        }
        private string role;

        public string Role
        {
            get { return role; }
        }
        private int strength;

        public int Strength
        {
            get { return strength; }
        }
        private string team;

        public string Team
        {
            get { return team; }
        }
        private int visRange;

        public int VisRange
        {
            get { return visRange; }
        }

        public override void ReadXml(System.Xml.XmlReader reader)
        {
            reader.MoveToContent();

            energy = Convert.ToInt32(reader["energy"]);
            health = Convert.ToInt32(reader["health"]);
            maxEnergy = Convert.ToInt32(reader["maxEnergy"]);
            maxHealth = Convert.ToInt32(reader["maxHealth"]);
            name = reader["name"];
            node = reader["node"];
            role = reader["role"];
            strength = Convert.ToInt32(reader["strength"]);
            team = reader["team"];
            visRange = Convert.ToInt32(reader["visRange"]);
        }
    }
}
