using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Dynamic;
using System.IO;
using System.Text.Json;
using System.Text.RegularExpressions;
using System.Xml;

namespace antlr4_fortran_parser
{
    class Program
    {
        static void Main(string[] args)
        {
            var file = args.Length == 0 || string.IsNullOrEmpty(args[0]) ? "dogtail.f" : args[0];
            var input = File.ReadAllLines(file);
            //
            // preprocess for linecont as antlr4 lex cant handle that (it splits either side into 2 sep tokens but
            // really just string concat
            //
            List<string> inputpp = new();
            int blanklines = 0;
            foreach (var l in input)
            {
                if (l.Length >= 6 && l[5] == '&')
                {
                    // add to end of prev line...
                    inputpp[inputpp.Count - 1] += l.Substring(6);
                    blanklines++;
                }
                else
                {
                    for (var i = 0; i < blanklines; i++)
                        inputpp.Add("");
                    blanklines = 0;
                    inputpp.Add(l);
                }
            }
            AntlrInputStream inputStream = new AntlrInputStream(string.Join("\r\n", inputpp));
            Fortran77Lexer lexer = new(inputStream);

            CommonTokenStream commonTokenStream = new CommonTokenStream(lexer);
            Fortran77Parser parser = new(commonTokenStream);

            //
            // parse input
            var res = parser.program();

            //
            // write result to file
            var format = 2;
            string extn = format switch
            {
                0 => ".lisp",
                1 => ".xml",
                2 => ".json",
                _ => throw new ArgumentOutOfRangeException("INvalid format"),
            };
            var resfile = file + extn;

            if (format == 0)
            {
                // LISP Like tree as single string
                string lispRes = res.ToStringTree();
                File.WriteAllText(resfile, lispRes);
            }
            else
            {
                using TextWriter wrt = File.CreateText(resfile);
                printNode(res, wrt, format, 0, true);
            }

            var fi = new FileInfo(resfile);
            Console.WriteLine($"File {fi.FullName} write complete; {fi.Length} bytes");

            if (format == 1)
            {
                // verify xml
                try
                {
                    XmlDocument x = new();
                    x.Load(resfile);
                    Console.WriteLine("...verified file!!");
                }
                catch (Exception e)
                {
                    Console.WriteLine("failed to verify file!! " + e.Message);
                }
            }
            else if (format == 2)
            {
                JsonDocument jd = null;
                try
                {
                    using var rd = File.OpenRead(resfile);
                    var jo = new JsonDocumentOptions { MaxDepth = 256 };
                    jd = JsonDocument.Parse(rd, jo);
                    Console.WriteLine("...verified file!!");

                }
                catch (Exception e)
                {
                    Console.WriteLine("failed to verify file!! " + e.Message);
                }

                processJson(jd, resfile);
            }
        }

        //
        // ignore these intermediate expressio nodes when its single child of a parent
        // OR just rename all to "expr" if more than one child
        static Regex rxIgnoreExpr = new Regex("(NcExpr|[LAI]expr[0-5(Code)])", RegexOptions.Compiled);

        static void printNode(IParseTree node, TextWriter wrt, int format, int depth, bool isLastChild)
        {
            var text = node.Payload;
            var type = node.GetType();
            var typeName = type.Name;
            var cc = node.ChildCount;
            bool ignore = false;
            var indent = "".PadLeft(depth * 2);
            var tail = "";

            switch (format)
            {
                case 2: if (!isLastChild) { tail = ","; } break;
            }

            if (node is ParserRuleContext)
            {
                var stripEnd = "Context";
                if (typeName.EndsWith(stripEnd))
                    typeName = typeName.Substring(0, typeName.Length - stripEnd.Length);
                else
                    Debug.Fail("Unexpected type?");

                bool isIgnoreExprMatch = rxIgnoreExpr.IsMatch(typeName);
                if (isIgnoreExprMatch)
                    typeName = "expr";

                // no children
                if (cc == 0)
                {
                    switch (format)
                    {
                        case 1: wrt.WriteLine($"{indent}<{typeName} />"); return;
                        case 2: wrt.WriteLine($"{indent}{{ \"{typeName}\": {{}} }}{tail}"); return;
                    }
                }
                // single child
                else if (cc == 1)
                {
                    // and if 
                    var c1 = node.GetChild(0);
                    if (c1 is ParserRuleContext)
                    {
                        // if its intermediate part of expression tree we're not intereted in...
                        if (isIgnoreExprMatch)
                            ignore = true;
                    }
                    else
                    {
                        // that child is a terminal node...
                        var st = c1.ToStringTree();
                        switch (format)
                        {
                            case 1: wrt.WriteLine($"{indent}<{typeName}>{st}</{typeName}>"); return;
                            case 2:
                                var encst = System.Text.Json.JsonEncodedText.Encode(st, null).ToString();
                                wrt.WriteLine($"{indent}{{ \"{typeName}\": \"{encst}\" }}{tail}"); return;
                        }
                    }
                }
                if (ignore)
                {
#if DEBUG_IGNORE
                    // could write a comment?
                    switch (format)
                    {
                        case 1: wrt.WriteLine($"{indent}<!-- {typeName} -->"); break;
                    }
#endif
                }
                else
                {
                    switch (format)
                    {
                        case 0: wrt.WriteLine($"{indent}{typeName}"); break;
                        case 1: wrt.WriteLine($"{indent}<{typeName}>"); break;
                        case 2: wrt.WriteLine($"{indent}{{ \"{typeName}\": ["); break;
                    }
                }
                for (int c = 0; c < cc; c++)
                {
                    // dont change depth or lastchild flag is ignoring parent
                    if (ignore)
                        printNode(node.GetChild(c), wrt, format, depth, isLastChild);
                    else
                        printNode(node.GetChild(c), wrt, format, depth + 1, c == cc - 1);
                }
                if (!ignore)
                {
                    switch (format)
                    {
                        case 1: wrt.WriteLine($"{indent}</{typeName}>"); break;
                        case 2: wrt.WriteLine($"{indent}] }}{tail}"); break;
                    }
                }
            }
            else
            {
                Debug.Assert(node.ChildCount == 0); // should only be a "TerminalNodeImpl" with no choldren
                var st = node.ToStringTree();
                switch (format)
                {
                    case 0:
                    case 1:
                        wrt.WriteLine($"{indent}{st}"); break;
                    case 2:
                        var encst = System.Text.Json.JsonEncodedText.Encode(st, null).ToString();
                        wrt.WriteLine($"{indent}\"{encst}\"{tail}"); break;
                }
            }
        }


        private static void processJson(JsonDocument jd, string resfile)
        {
            var jo = new JsonSerializerOptions
            {
                MaxDepth = 256,
                WriteIndented = true,
                Converters = { new KVNodeConverter() }
            };
            var jwo = new JsonWriterOptions { Indented = true };

            var res1file = resfile.Replace(".json", ".1.json");
            using var wrt1 = File.CreateText(res1file);
            JsonSerializer.Serialize(new Utf8JsonWriter(wrt1.BaseStream, jwo), jd, jo);
            var fi1 = new FileInfo(res1file);
            Console.WriteLine($"File {fi1.FullName} write complete; {fi1.Length} bytes");

            object model = processJsonElement(jd.RootElement);
            Debug.Assert(model is KVNode);

            var res2file = resfile.Replace(".json", ".2.json");
            using var wrt2 = File.CreateText(res2file);
            JsonSerializer.Serialize(new Utf8JsonWriter(wrt2.BaseStream, jwo), model, jo);

            var fi2 = new FileInfo(res2file);
            Console.WriteLine($"File {fi2.FullName} write complete; {fi2.Length} bytes");

            var res3file = resfile.Replace(".json", ".3.json");
            using var wrt3 = File.CreateText(res3file);
            new KVNodePrint(wrt3).WriteObjectValue(model as KVNode, 0);

            var fi3 = new FileInfo(res3file);
            Console.WriteLine($"File {fi3.FullName} write complete; {fi3.Length} bytes");
        }


        private static object processJsonElement(JsonElement jn)
        {
            var vk = jn.ValueKind;
            switch (vk)
            {
                case JsonValueKind.Object:
                    {
                        KVNode retkv = null;
                        foreach (var jnn in jn.EnumerateObject())
                        {
                            var k = jnn.Name;
                            var v = jnn.Value;
                            var p = processJsonElement(v);
                            Debug.Assert(retkv == null); // only ever one key in my objects in this model!
                            retkv = new(k, p);
                        }
                        if (retkv == null)
                            return null;
                        else
                            return retkv.optimize();
                    }

                case JsonValueKind.Array:
                    {
                        ArrayList<object> reta = new();
                        foreach (var jnv in jn.EnumerateArray())
                        {
                            var mv = processJsonElement(jnv);
                            if (mv != null)
                            {
                                var mvsa = mv as KVNodeSiblingArray;
                                if (mvsa != null)
                                {
                                    for (int c = 0; c < mvsa.ChildCount; c++)
                                    {
                                        reta.Add(mvsa.GetChild(c));
                                    }
                                }
                                else
                                    reta.Add(mv);
                            }
                        }
                        return reta;
                    }

                case JsonValueKind.String:
                    return jn.GetString();

                case JsonValueKind.Null:
                    return null;
            }
            throw new Exception("Unexpected node type?");
        }


    }

}
