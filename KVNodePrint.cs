using Antlr4.Runtime.Misc;
using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;

namespace antlr4_fortran_parser
{
    public class KVNodePrint
    {
        public KVNodePrint(StreamWriter wrt)
        {
            this.wrt = wrt;
        }

        StreamWriter wrt;
        bool bol = true;// at "beginning of line"

        public void WriteObjectValue(object obj, int depth)
        {
            if (obj == null)
                WriteNullValue();
            else if (obj is KVNode)
                WriteNode(obj as KVNode, depth);
            else if (obj is KVDict)
                WriteDictValue(obj as KVDict, depth);
            else if (obj is ArrayList<object>)
                WriteArrayValue(obj as ArrayList<object>, depth + 1);
            else if (obj is int)
                WriteNumberValue((int)obj);
            else if (obj is double)
                WriteNumberValue((double)obj);
            else if (obj is string)
                WriteStringValue(obj.ToString());
            else
                throw new InvalidOperationException();
        }

        public void WriteNode(KVNode kvn, int depth)
        {
            WriteIndent(depth);
            Write($"{{ \"{kvn.Key}\":");
            bool singleline = childNodesAreSimple(kvn);
            WriteIndent(depth);
            WriteObjectValue(kvn.Value, depth);
            if (!singleline)
                WriteLine();
            WriteIndent(depth);
            Write("}");
        }

        bool childNodesAreSimple(KVNode kvn)
        {
            int cc = kvn.ChildCount;
            for (int c = 0; c < cc; c++)
            {
                var cn = kvn.GetChild(c);
                if (cn is KVNode)
                {
                    if (c == 0)
                        return childNodesAreSimple(cn as KVNode);
                    else
                        return false;
                }
            }
            return true;
        }

        public void WriteDictValue(KVDict value, int depth)
        {
            WriteIndent(depth);
            Write($"{{ ");
            WriteLine();
            var d = value as KVDict;
            int c = d.Count;
            int n = 0;
            foreach (DictionaryEntry e in d)
            {
                string k = e.Key.ToString();
                WriteIndent(depth+1);
                Write($"\"{k}\": ");
                WriteObjectValue(e.Value, depth+1);
                if (++n < c)
                    Write(",");
                WriteLine();
            }
            WriteIndent(depth);
            Write($"}}");
            WriteLine();
        }

        public void WriteArrayValue(ArrayList<object> value, int depth)
        {
            bool singleline = true;
            foreach (var item in value)
            {
                if (item is KVNode)
                {
                    if ((item as KVNode).ChildCount > 1)
                        singleline = false;
                }
                else if (item is ArrayList<object>)
                {
                    singleline = false;
                }
            }

            Write("[");
            if (!singleline)
                WriteLine();
            bool first = true;
            foreach (var item in value)
            {
                if (first)
                    first = !first;
                else
                {
                    Write(",");
                    if (!singleline)
                        WriteLine();
                }
                WriteIndent(depth);
                WriteObjectValue(item, depth);
            }
            if (!singleline)
                WriteLine();
            WriteIndent(depth);
            Write("]");
            if (!singleline)
                WriteLine();
        }

        public void WriteNullValue()
        {
            Write("{}");
        }

        public void WriteStringValue(string value)
        {
            var encvalue = JsonEncodedText.Encode(value, null).ToString();
            Write("\"" + encvalue + "\"");
        }

        public void WriteNumberValue(int value)
        {
            Write(value.ToString());
        }

        public void WriteNumberValue(double value)
        {
            Write(value.ToString());
        }

        public void Write(string value)
        {
            wrt.Write(value);
            bol = false;
        }

        public void WriteLine()
        {
            wrt.WriteLine();
            bol = true;
        }

        public void WriteIndent(int depth)
        {
            if (bol)
                wrt.Write("".PadLeft(depth));
            else if (depth > 0)
                wrt.Write(" ");
        }
    }
}
