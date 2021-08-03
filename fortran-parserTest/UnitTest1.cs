using NUnit.Framework;
using antlr4_fortran_parser;
using System.IO;
using System.Diagnostics;
using System.Text;
using System;

namespace fortran_parserTest
{
    public class Tests
    {
        [SetUp]
        public void Setup()
        {
        }

        [Test]
        public void Test_program_on_dogtail()
        {
            var f = "dogtail.f";
            Assert.IsTrue(File.Exists(f));
            Program.Main(new string[] { f });
            Assert.IsTrue(File.Exists(f + ".json"));
            Assert.IsTrue(File.Exists(f + ".2.json"));

            test_JSONPath(f, "$.Program.IO[0].SubroutineStatement[0].Name", $"\"IO\"{EOL}");
        }

        const string EOL = "\r\n";

        void test_JSONPath(string file, string query, string expected)
        {
            StringBuilder sb = new();
            using StringWriter wrt = new(sb);
            Program.JSONPathOutput = wrt;
            Program.Main(new string[] { "-n", file, $"{query}" });
            string res = sb.ToString();

            // convert expected eol to current writer eol
            var nl = wrt.NewLine;
            if (nl != EOL)
                expected = expected.Replace(EOL, nl);

            Assert.AreEqual(expected, res);
        }
    }
}