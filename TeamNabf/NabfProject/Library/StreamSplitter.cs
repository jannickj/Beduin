using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;

namespace NabfProject.Library
{
    public class StreamSplitter : Stream 
    {
        private Stream mainStream;
        private Stream returnStream;

        public StreamSplitter(Stream mainStream, Stream returnStream)
        {
            this.mainStream = mainStream;
            this.returnStream = returnStream;
        }



        public override bool CanRead
        {
            get { return this.mainStream.CanRead; }
        }

        public override bool CanSeek
        {
            get { return this.mainStream.CanSeek; }
        }

        public override bool CanWrite
        {
            get { return this.mainStream.CanWrite; }
        }

        public override void Flush()
        {
            this.mainStream.Flush();
        }

        public override long Length
        {
            get { return this.mainStream.Length; }
        }

        public override long Position
        {
            get
            {
                return this.mainStream.Position;
            }
            set
            {
                this.mainStream.Position = value;
            }
        }

        public override int Read(byte[] buffer, int offset, int count)
        {
            var data = this.mainStream.Read(buffer, offset, count);
            this.returnStream.Write(buffer, offset, data);
            return data;
        }

        public override long Seek(long offset, SeekOrigin origin)
        {
            return this.mainStream.Seek(offset, origin);
        }

        public override void SetLength(long value)
        {
            this.mainStream.SetLength(value);
        }

        public override void Write(byte[] buffer, int offset, int count)
        {
            this.mainStream.Write(buffer, offset, count);
        }
    }
}
