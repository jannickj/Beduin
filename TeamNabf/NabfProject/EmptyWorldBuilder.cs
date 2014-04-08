using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using XmasEngineModel;

namespace NabfProject
{
    public class EmptyWorldBuilder : XmasWorldBuilder
    {
        protected override XmasWorld ConstructWorld()
        {
            return new EmptyWorld();
        }
    }
}
