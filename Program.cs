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
        static string file = null;
        static int format = 2;
        static int jformat = 2;
        static string jquery = null;
        static bool noparse = false;
        static bool help = false;
        static int verbose = 0;

        static void Main(string[] args)
        {
            try
            {

                int nf = 0;
                for (int ax = 0; ax < args.Length; ax++)
                {
                    string a = args[ax];
                    if (a == "-h" || a == "--help")
                        help = true;
                    else if (a == "-v" || a == "--verbose")
                        verbose = 1;
                    else if (a == "-n" || a == "--noparse")
                        noparse = true;
                    else if (a == "-f" || a == "--format")
                        format = int.Parse(args[++ax]);
                    else if (a == "-j" || a == "--jformat")
                        jformat = int.Parse(args[++ax]);
                    else if (a == "-p" || a == "--json-path")
                        jquery = args[++ax];
                    else
                    {
                        switch (nf++)
                            {
                            case 0: file = a; break;
                            case 1: jquery = a; break;
                            default: throw new ArgumentException();
                        }
                    }
                }
            }
            catch
            {
                Console.WriteLine("Argument error");
                help = true;
            }

            if (help || string.IsNullOrEmpty(file))
            {
                Console.WriteLine(
                    @"
fortran-parser [args] [file] [JSONPath]
args:
    --help      print this help
    --format    raw output format 0:LISP, 1:XML, 2:JSON
    --jformat   processed JSON file format; 0:none, 1:normal 2:optimized
    --noparse   do not reparse fortran file just reuse previous results for new query
    --verbose   print some detail as we go along
    [file]      FORTRAN input file name (ends in .f ...result file will be same name replacing with .json etc)
    [JSONPath]  (json only) select tokens from processed result and print
");
                return;
            }
            //
            // write result to file
            string extn = Math.Abs(format) switch
            {
                0 => ".lisp",
                1 => ".xml",
                2 => ".json",
                _ => throw new ArgumentOutOfRangeException("INvalid format"),
            };
            var resfile = file + extn;

            if (!noparse)
            {

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
            }

            if (verbose > 0)
            {
                var fi = new FileInfo(resfile);
                Console.WriteLine($"File {fi.FullName} write complete; {fi.Length} bytes");
            }

            if (format == 1)
            {
                // verify xml
                try
                {
                    XmlDocument x = new();
                    x.Load(resfile);
                    if (verbose > 0)
                        Console.WriteLine("...verified file!!");
                }
                catch (Exception e)
                {
                    Console.WriteLine("failed to verify file!! " + e.Message);
                }
            }
            else if (format == 2 || format == -2)
            {
                JsonDocument jd = null;
                try
                {
                    using var rd = File.OpenRead(resfile);
                    var jo = new JsonDocumentOptions { MaxDepth = 256 };
                    jd = JsonDocument.Parse(rd, jo);
                    if (verbose > 0)
                        Console.WriteLine("...verified file!!");

                }
                catch (Exception e)
                {
                    Console.WriteLine("failed to verify file!! " + e.Message);
                }

                if (jformat > 0)
                    processJson(jformat, jd, resfile, jquery);
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


        private static void processJson(int jformat, JsonDocument jd, string resfile, string jquery)
        {
            var jo = new JsonSerializerOptions
            {
                MaxDepth = 256,
                WriteIndented = true,
                Converters = { new KVNodeConverter() }
            };
            var jwo = new JsonWriterOptions { Indented = true };

            object model = processJsonElement(jd.RootElement);
            Debug.Assert(model is KVNode);

            var resjfile = resfile.Replace(".json", $".{jformat}.json");
            using var wrtj = File.CreateText(resjfile);

            switch (jformat)
            {
                case 0:
                    {
                        JsonSerializer.Serialize(new Utf8JsonWriter(wrtj.BaseStream, jwo), jd, jo);
                        break;
                    }
                case 1:
                    {
                        JsonSerializer.Serialize(new Utf8JsonWriter(wrtj.BaseStream, jwo), model, jo);
                        break;
                    }
                case 2:
                    {
                        new KVNodePrint(wrtj).WriteObjectValue(model as KVNode, 0);
                        break;
                    }
            }
            wrtj.Close();
            if (verbose > 0)
            {
                var fij = new FileInfo(resjfile);
                Console.WriteLine($"File {fij.FullName} write complete; {fij.Length} bytes");
            }

            try
            {
                var nsRdr = new Newtonsoft.Json.JsonTextReader(File.OpenText(resjfile));
                var nsSerializer = new Newtonsoft.Json.JsonSerializer();
                var nsRes = nsSerializer.Deserialize(nsRdr);
                if (verbose > 0)
                    Console.WriteLine("...verified");

                if (!string.IsNullOrEmpty(jquery))
                {
                    var nsjo = (Newtonsoft.Json.Linq.JObject)nsRes;
                    var nsSelectRes = nsjo.SelectTokens(jquery);
                    foreach (var token in nsSelectRes)
                    {
                        Console.WriteLine(token.ToString());
                    }
                }
            }
            catch (Exception e)
            {
                Console.WriteLine($"...verified failed on {resjfile}: {e.Message}");
            }

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
