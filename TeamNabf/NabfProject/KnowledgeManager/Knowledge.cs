using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace NabfProject.KnowledgeManagerModel
{
    public interface Knowledge : IEquatable<Knowledge>, IComparable<Knowledge>
    {
        string GetTypeToString();

        
    }
}
