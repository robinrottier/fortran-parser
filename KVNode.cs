using Antlr4.Runtime.Misc;
using System;
using System.Diagnostics;
using System.Text;

namespace antlr4_fortran_parser
{
    public class KVNode
    {
        public string Key { get; set; }
        public object Value { get; set; }

        public KVNode(string k, object v) { Key = k; Value = v; }

        bool IsValueNull { get => Value == null; }
        bool IsValueString { get => Value is string; }
        bool IsValueNode { get => Value is KVNode; }
        bool IsValueArray { get => Value is ArrayList<object>; }

        int ChildCount
        {
            get
            {
                if (IsValueArray) return (Value as ArrayList<object>).Count;
                if (IsValueNode) return 1;
                if (IsValueString) return 1;
                return 0;
            }
        }

        ArrayList<object> ArrayValue { get { return Value as ArrayList<object>; } }

        object GetChild(int i)
        {
            if (i >= 0)
            {
                if (IsValueNode || IsValueString)
                {
                    if (i == 0)
                        return Value;
                }
                else if (IsValueArray)
                {
                    var a = ArrayValue;
                    if (i < a.Count)
                        return a[i];
                }
            }
            throw new Exception("Child index out of range");
        }

        void InsertChild(int i, object value)
        {
            if (i < 0 || i >= ChildCount)
                throw new ArgumentOutOfRangeException();
            // convert node to array with node
            if (IsValueString || IsValueNode)
            {
                Value = new ArrayList<object> { Value };
            }
            ArrayValue.Insert(i, value);
        }

        void RemoveChild(int i)
        {
            if (i < 0 || i >= ChildCount)
                throw new ArgumentOutOfRangeException();
            if (IsValueArray)
            {
                var a = ArrayValue;
                a.RemoveAt(i);
                return;
            }
            Debug.Fail("Remving child btu not an array?");
            Value = null;
        }

        void AppendStringText(StringBuilder sb)
        {
            int cc = ChildCount;
            for (int c = 0; c < cc; c++)
            {
                var cs = GetChild(c);
                if (cs is KVNode)
                {
                    (cs as KVNode).AppendStringText(sb);
                }
                else if (cs is string)
                {
                    sb.Append(' ');
                    sb.Append(cs);
                }
            }
        }

        public KVNode optimize()
        {
            int cc = ChildCount;
            switch (Key)
            {
                //
                // Program has some blank line nodes...remove them
                case "Program":
                    {
                        for (int c = 0; c < cc; c++)
                        {
                            var cn = GetChild(c);
                            if (cn == null
                             || cn is string && string.IsNullOrWhiteSpace(cn.ToString()))
                            {
                                RemoveChild(c);
                                cc--;
                            }
                        }
                        return this;
                    }
                //
                // Comments...just remove
                case "CommentStatement":
                    return null;
                //
                // these ndes COULD be empty, especially after other optimizes
                // and then can be removed
                case "MainProgram":
                case "SubprogramBody":
                    {
                        if (cc == 0)
                            return null;
                        break;
                    }
                // 
                // these nodes expect just have single child...so just use that child
                // (and throw if not)
                case "ExecutableUnit":
                case "Statement":
                    {
                        if (cc == 1)
                        {
                            object c0 = GetChild(0);
                            if (c0 is KVNode)
                                return c0 as KVNode;
                        }
                        else if (cc == 0)
                            return null;
                        throw new Exception($"UNexpected in {Key}");
                    }
                //
                // "whole statement" can have a label and/or EOLs
                // -- loose the EOL
                // -- add the ;abe to child statment
                case "WholeStatement":
                    {
                        if (cc == 3)
                        {
                            //
                            // shoud be LABEL/statement/EOL
                            object c0 = GetChild(0);
                            object c1 = GetChild(1);
                            object c2 = GetChild(2);

                            if (c1 is KVNode && string.IsNullOrWhiteSpace(c2.ToString()))
                            {
                                // extract label and add it to the statement
                                var ret = c1 as KVNode;
                                ret.InsertChild(0, new KVNode("LABEL", c0));
                                return ret;
                            }
                        }
                        else if (cc == 2)
                        {
                            //
                            // shoud be statement/EOL
                            object c0 = GetChild(0);
                            object c1 = GetChild(1);

                            if (c0 is KVNode && string.IsNullOrWhiteSpace(c1.ToString()))
                                return c0 as KVNode;
                        }
                        else if (cc == 1)
                        {
                            object c0 = GetChild(0);
                            if (c0 is KVNode)
                                return c0 as KVNode;
                        }
                        else if (cc == 0)
                            return null;
                        throw new Exception($"UNexpected in {Key}");
                    }
                //
                // these statememtns we dont really care about
                //
                // just want to presresent so we know whats what
                case "ImplicitStatement":
                case "TypeStatement":
                case "ExternalStatement":
                case "FormatStatement":
                    {
                        StringBuilder sb = new();
                        AppendStringText(sb);
                        return new KVNode(Key, sb.ToString());
                    }
            }
            return this;
        }

    };

}
