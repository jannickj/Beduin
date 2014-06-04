using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace NabfProject.KnowledgeManagerModel
{
    public interface TargetedKnowledge : Knowledge
    {
        string TargetedAgent { get; }
    }
}
