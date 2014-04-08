using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using XmasEngineModel;
using XmasEngineModel.EntityLib;

namespace NabfProject
{
    public class EmptyWorld : XmasWorld
    {
        protected override bool OnAddEntity(XmasEngineModel.EntityLib.XmasEntity xmasEntity, XmasEngineModel.World.EntitySpawnInformation info)
        {
            return true;
        }

        protected override void OnRemoveEntity(XmasEngineModel.EntityLib.XmasEntity entity)
        {

        }

        public override XmasEngineModel.World.XmasPosition GetEntityPosition(XmasEngineModel.EntityLib.XmasEntity xmasEntity)
        {
            return null;
        }

        public override bool SetEntityPosition(XmasEngineModel.EntityLib.XmasEntity xmasEntity, XmasEngineModel.World.XmasPosition tilePosition)
        {
            return true;
        }

        public override ICollection<XmasEngineModel.EntityLib.XmasEntity> GetEntities(XmasEngineModel.World.XmasPosition pos)
        {
            return new XmasEntity[0];
        }
    }
}
