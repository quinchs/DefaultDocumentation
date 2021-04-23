using System;
using System.Collections.Generic;
using System.Linq;

namespace DefaultDocumentation.Helper
{
    internal static class IEnumerableExtension
    {
        public static T[] ToArrayOrNull<T>(this IEnumerable<T> enumerable)
        {
            T[] array = enumerable?.ToArray() ?? Array.Empty<T>();

            return array.Length > 0 ? array : null;
        }
    }
}
