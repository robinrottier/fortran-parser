using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using System;
using System.Collections.Generic;
using System.Diagnostics;
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
            Fortran77Lexer lexer = new (inputStream);

            //var lexErrorHandler = new MyErrorListener();
            //lexer.RemoveErrorListeners();
            //lexer.AddErrorListener(lexErrorHandler);

            CommonTokenStream commonTokenStream = new CommonTokenStream(lexer);
            Fortran77Parser parser = new (commonTokenStream);

            //parser.RemoveErrorListeners();
            //parser.AddErrorListener(new MyErrorListener());

            //MyListener listener = new ();
            //parser.AddParseListener(listener);

            // what does this do? --nothing happens without it!
            var res = parser.program();

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
                printRootNode(res, wrt, format);
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
                try
                {
                    using var rd = File.OpenRead(resfile);
                    var jo = new JsonDocumentOptions { MaxDepth = 256 };
                    var jd = JsonDocument.Parse(rd, jo);
                    Console.WriteLine("...verified file!!");
                }
                catch (Exception e)
                {
                    Console.WriteLine("failed to verify file!! " + e.Message);
                }
            }
        }

        static void printRootNode(IParseTree node, TextWriter wrt, int format)
        {
            switch (format)
            {
                //case 2: wrt.WriteLine("{"); break;
            }
            printNode(node, wrt, format, 1, true);
            switch (format)
            {
                //case 2: wrt.WriteLine("}"); break;
            }
        }

        static Regex rxIgnore = new Regex("(NcExpr|[LA]expr[0-5])", RegexOptions.Compiled);

        static void printNode(IParseTree node, TextWriter wrt, int format, int depth, bool isLastChild)
        {
            var text = node.Payload;
            var type = node.GetType();
            var typeName = type.Name;
            var cc = node.ChildCount;
            bool ignore = false;
            var indent = "".PadLeft(depth);
            var tail = "";

            switch (format)
            {
                case 2: if (!isLastChild) { tail = ","; } break;
            }

            if (node is ParserRuleContext)
            {
                var stripFront = "Fortran77Parser+";
                var stripEnd = "Context";
                if (typeName.StartsWith(stripFront))
                    typeName = typeName.Substring(stripFront.Length);
                if (typeName.EndsWith(stripEnd))
                    typeName = typeName.Substring(0, typeName.Length - stripEnd.Length);
                else
                    Debug.Fail("Unexpected type?");

                bool isIgnoreMatch = rxIgnore.IsMatch(typeName);
                if (isIgnoreMatch)
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
                        if (isIgnoreMatch)
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
                        printNode(node.GetChild(c), wrt, format, depth + 1, c == cc-1);
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

        public class MyErrorListener : BaseErrorListener
        {
            public override void SyntaxError(TextWriter output, IRecognizer recognizer, IToken offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
            {
                Debug.WriteLine("SyntaxError: ");
                base.SyntaxError(output, recognizer, offendingSymbol, line, charPositionInLine, msg, e);
            }
        }

        public class MyListener : Fortran77ParserBaseListener
        {
            public override void EnterEveryRule([NotNull] ParserRuleContext context)
            {
                Debug.WriteLine("Enter: "+ context.Start.ToString());

            }

            /// <inheritdoc/>
            /// <remarks>The default implementation does nothing.</remarks>
            public override void ExitEveryRule([NotNull] ParserRuleContext context)
            {
            }

            public override void VisitErrorNode([NotNull] IErrorNode node)
            {
                Debug.WriteLine("Error: ");
                base.VisitErrorNode(node);
            }
        }

    }
}
