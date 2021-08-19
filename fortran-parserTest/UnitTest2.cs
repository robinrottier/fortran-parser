using NUnit.Framework;
using antlr4_fortran_parser;
using System.IO;
using System.Diagnostics;
using System.Text;
using System;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace fortran_parserTest
{
    public class UnitTest2
    {
        [SetUp]
        public void Setup()
        {
        }

        [Test]
        public void Test1()
        {
            test_simple_expr("A=3", "A", "[3]");
            test_simple_expr("A=3+4", "A", "[3,+,4]");
            test_simple_expr("A=Q3", "A", "[Q3]");
            test_simple_expr("A=Q3(5)", "A", "[[Q3,5]]");
            test_simple_expr("A=Q3+5", "A", "[Q3,+,5]");
            test_simple_expr("A=Q3(5)+6", "A", "[[Q3,5],+,6]");
            test_simple_expr("A=Q3(5)+B3", "A", "[[Q3,5],+,B3]");
        }

        public void test_simple_expr(string source, string expectedlhs, string expectedrhs)
        {
            var f = "temp.test.f";
            if (File.Exists(f))
            {
                File.Delete(f);
            }
            Assert.False(File.Exists(f));

            // write dummy file continaing single expression...need the END to make it valid
            File.WriteAllText(f, $"      {source}{EOL}      END{EOL}");
            var fi = new FileInfo(f);
            Debug.WriteLine($"Written file {fi.FullName} with code {source}");

            // and parse it
            Program.Main(new string[] { f });

            // check we got a result....
            Assert.IsTrue(File.Exists(f + ".json"));
            Assert.IsTrue(File.Exists(f + ".2.json"));

            // read result
            using var rdr = File.OpenText(f + ".2.json");
            using var nsRdr = new Newtonsoft.Json.JsonTextReader(rdr);
            var nsSerializer = new Newtonsoft.Json.JsonSerializer();
            var nsRes = nsSerializer.Deserialize(nsRdr);
            var nsjo = (Newtonsoft.Json.Linq.JObject)nsRes;

            nsRdr.Close();
            rdr.Close();
            File.Delete(f + ".json");
            File.Delete(f + ".2.json");

            // check its all as expected and get lhs/rhs
            Assert.IsTrue(nsjo.Count == 1);
            Assert.IsTrue(nsjo.ContainsKey("Program"));
            var nsjop = nsjo["Program"] as JObject;
            Assert.IsTrue(nsjop.Count == 1);
            Assert.IsTrue(nsjop.ContainsKey("Main"));
            var nsjopm = nsjop["Main"] as JArray;
            Assert.IsTrue(nsjopm != null);
            Assert.IsTrue(nsjopm.Count == 2);

            var nsjopm0 = nsjopm[0] as JObject;
            Assert.IsNotNull(nsjopm0);
            Assert.IsTrue(nsjopm0.Count == 1);
            Assert.IsTrue(nsjopm0.ContainsKey("AssignmentStatement"));
            var nsjopm0as = nsjopm0["AssignmentStatement"] as JObject;
            
            Assert.IsTrue(nsjopm0as.Count == 2);
            Assert.IsTrue(nsjopm0as.ContainsKey("lhs"));
            Assert.IsTrue(nsjopm0as.ContainsKey("rhs"));

            var lhs = nsjopm0as["lhs"];
            Assert.AreEqual(expectedlhs, lhs.ToString());

            var rhs = nsjopm0as["rhs"];
            var xrhs = rhs.ToString();
            xrhs = xrhs.Replace("\n", "");
            xrhs = xrhs.Replace("\r", "");
            xrhs = xrhs.Replace(" ", "");// carefull!! make sur ewe dont expect spaces
            xrhs = xrhs.Replace("\"", "");// carefull!! make sur ewe dont expect "
            Assert.AreEqual(expectedrhs, xrhs);
        }

        const string EOL = "\r\n";
    }
}