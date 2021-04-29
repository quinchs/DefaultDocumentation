using System;
using System.Collections.Concurrent;
using System.Text;

namespace DefaultDocumentation.Helper
{
    internal static class StringBuilderPool
    {
        internal class Disposable : IDisposable
        {
            private readonly StringBuilder _builder;

            public Disposable(StringBuilder builder)
            {
                _builder = builder;
            }

            public void Dispose()
            {
                _builder.Clear();
                _builders.Push(_builder);
            }
        }

        private static readonly ConcurrentStack<StringBuilder> _builders = new();

        public static IDisposable Get(out StringBuilder builder)
        {
            if (!_builders.TryPop(out builder))
            {
                builder = new StringBuilder();
            }

            return new Disposable(builder);
        }
    }
}
