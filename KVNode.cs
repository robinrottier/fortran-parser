using Antlr4.Runtime.Misc;
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
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
        bool IsValueInt { get => Value is int; }
        bool IsValueDouble { get => Value is double; }
        bool IsValueNode { get => Value is KVNode; }
        bool IsValueArray { get => Value is ArrayList<object>; }
        bool IsValueDict { get => Value is KVDict; }

        public int ChildCount
        {
            get
            {
                if (IsValueArray) return ArrayValue.Count;
                if (IsValueDict) return DictValue.Count;
                if (IsValueNull) return 0;
                return 1;
            }
        }

        ArrayList<object> ArrayValue { get { return Value as ArrayList<object>; } }
        KVDict DictValue { get { return Value as KVDict; } }

        public object GetChild(int i)
        {
            if (i >= 0)
            {
                if (IsValueArray)
                {
                    var a = ArrayValue;
                    if (i < a.Count)
                        return a[i];
                }
                else if (IsValueDict)
                {
                    return DictValue[i];
                }
                else if (IsValueNull)
                {
                }
                else
                {
                    if (i == 0)
                        return Value;
                }
            }
            throw new ArgumentOutOfRangeException("Child index out of range");
        }

        void InsertChild(int i, object value)
        {
            if (i < 0 || i >= ChildCount)
                throw new ArgumentOutOfRangeException();
            // convert node to array with node
            if (IsValueDict)
                throw new InvalidOperationException("Cant convert dictionary value into array");
            if (!IsValueArray)
            {
                Value = new ArrayList<object> { Value };
            }
            ArrayValue.Insert(i, value);
        }

        void AddChild(object value)
        {
            if (IsValueDict)
                throw new InvalidOperationException("Cant convert dictionary value into array");
            // convert node to array with node
            if (IsValueNull)
            {
                Value = new ArrayList<object>();
            }
            else if (IsValueArray)
            {
            }
            else
            {
                Value = new ArrayList<object> { Value };
            }
            ArrayValue.Add(value);
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

        public string ToStringText(string sep = " ")
        {
            var sb = new StringBuilder();
            AppendStringText(sb, sep);
            return sb.ToString();
        }

        void AppendStringText(StringBuilder sb, string sep = " ")
        {
            int cc = ChildCount;
            for (int c = 0; c < cc; c++)
            {
                var cs = GetChild(c);
                AppendStringText(sb, cs, sep);
            }
        }

        static void AppendStringText(StringBuilder sb, object obj, string sep = " ")
        {
            if (obj == null)
            {
                return;
            }
            else if (obj is KVNode)
            {
                (obj as KVNode).AppendStringText(sb, sep);
            }
            else if (obj is ArrayList<object>)
            {
                var a = obj as ArrayList<object>;
                foreach (var i in a)
                    AppendStringText(sb, i, sep);
            }
            else
            {
                if (sb.Length > 0)
                    sb.Append(sep);

                if (obj is string)
                    sb.Append(obj);
                else if (obj is int)
                    sb.Append(((int)obj).ToString());
                else if (obj is double)
                    sb.Append(((double)obj).ToString());
                else
                    Debug.Fail("Unexpected type in AppendStirngText");
            }
        }


        string GetChildStringText(int c, string sep = " ")
        {
            var n = GetChild(c);
            if (n is string)
                return n.ToString();
            var kvn = n as KVNode;
            if (kvn != null)
                return kvn.ToStringText(sep);
            throw new Exception("Expecting child string or node");
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
                        //
                        // should be left with one "MainProgram" and multiple subs
                        // so reorganize to look better
                        //
                        var ret = new KVDict();
                        var mp = ValidateChildNode(0, "MainProgram");
                        ret["Main"] = mp.Value;
                        //
                        for (int c = 1; c < cc; c++)
                        {
                            var sp = ValidateChildNode(c, "SubroutineSubprogram");
                            var ss = sp.ValidateChildNode(0, "SubroutineStatement");
                            var name = ss.ValidateChildNode(0, "Name").ValidateChildString(0);

                            ret[name] = sp.Value;
                        }
                        //
                        return new KVNode(Key, ret);
                    }
                //
                // Comments...just remove
                case "CommentStatement":
                    return null;
                //
                // these nodes COULD be empty, especially after other and then can be removed
                case "MainProgram":
                    {
                        if (cc == 0)
                            return null;
                        break;
                    }
                //
                // these nodes COULD be empty and then can be removed or just return all their children
                case "SubprogramBody":
                    {
                        if (cc == 0)
                            return null;
                        return new KVNodeSiblingArray(Value);
                    }
                // 
                // these nodes expect just have single child...so just use that child
                // (and throw if not)
                case "ExecutableUnit":
                case "Statement":
                case "OtherSpecificationStatement":
                case "ExecutableStatement":
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
                // these nodes expect just have single child node (expr) or a value
                // ...so just use that child                 // (and throw if not)
                case "Expression":
                    {
                        if (cc == 1)
                        {
                            object c0 = GetChild(0);
                            if (c0 is KVNode)
                            {
                                var kv0 = c0 as KVNode;
                                if (kv0.Key != "expr" && kv0.Key != "VarRef")
                                    ValidateFail($"expecting child node to be varref or expr (was {kv0.Key})");
                                //
                                // flatten expression tree into array
                                var a = new List<object>();
                                addExprTreeToArray(c0, a);
                                var ret = new KVNode("expr", null);
                                foreach (var x in a)
                                    ret.AddChild(x);
                                return ret;
                            }
                            else
                                return new KVNodeSiblingArray(c0);
                        }
                        else if (cc == 0)
                            return null;
                        throw new Exception($"Unexpected in {Key}");
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
                                // extract label and add it before statement
                                var ret = new KVNodeSiblingArray();
                                ret.AddChild(new KVNode("LABEL", c0));
                                ret.AddChild(c1);
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
                // just want to represent what was there so we know whats what
                case "ImplicitStatement":
                case "TypeStatement":
                case "ExternalStatement":
                case "FormatStatement":
                case "OpenStatement":
                case "ReadStatement":
                case "WriteStatement":
                case "CallStatement":
                case "IfStatement":
                    {
                        return new KVNode(Key, ToStringText());
                    }

                case "DimensionStatement":
                    {
                        // shoudl have "DIMENSION" and object childs only
                        ValidateChildCount(2);
                        ValidateChildString(0, "DIMENSION");
                        var c1 = ValidateChildNode(1, "ArrayDeclarators");

                        // should have one child "ArrayDeclarators" containing one of more ArrayDeclarator
                        var ret = new KVNode(Key, null);
                        for (int c = 0; c < c1.ChildCount; c++)
                        {
                            var c1c = c1.GetChild(c);
                            if (c1c is string && c1c.ToString() == ",")
                                continue;
                            var a1 = c1c as KVNode;
                            if (a1 == null || a1.Key != "ArrayDeclarator")
                                throw new Exception("Dimension statement expecting ArrayDeclarator");

                            var a1c0 = a1.GetChild(0);
                            if (!(a1c0 is string))
                                throw new Exception("ArrayDeclarator statement expecting string at position 0");
                            var arrayName = a1c0.ToString();

                            var a1c1 = a1.GetChild(1);
                            if (!(a1c1 is string) || a1c1.ToString() != "(")
                                throw new Exception("ArrayDeclarator statement expecting '(' at position 1");

                            var a1c2 = a1.GetChild(2) as KVNode;
                            if (a1c2 == null)
                                throw new Exception("ArrayDeclarator expecting node at position 2");

                            var aa = a1c2.ToStringText("").Split(",");
                            if (aa.Length > 1)
                            {
                                var arrayDim = new ArrayList<object>();
                                arrayDim.AddRange(aa);
                                var retv = new KVNode(arrayName, arrayDim);
                                ret.AddChild(retv);
                            }
                            else if (aa.Length == 1)
                            {
                                var retv = new KVNode(arrayName, aa[0]);
                                ret.AddChild(retv);
                            }
                            else
                                throw new Exception("ArrayDeclarator expecting extents; empty");
                        }
                        return ret;
                    }

                case "CommonStatement":
                    {
                        // shoudl have "COMMON" and object childs only
                        ValidateChildCount(2);
                        ValidateChildString(0, "COMMON");
                        var c1 = ValidateChildNode(1, "CommonBlock");

                        // commonblock shoudl have commonname and array of commonitem's
                        var c11 = c1.ValidateChildNode(0, "CommonName");
                        c11.ValidateChildCount(3);
                        c11.ValidateChildString(0, "/");
                        var blockName = c11.ValidateChildString(1);
                        c11.ValidateChildString(2, "/");

                        var c12 = c1.ValidateChildNode(1, "CommonItems");
                        var c12cc = c12.ChildCount;
                        var items = new ArrayList<object>();
                        for (int c = 0; c < c12cc; c++)
                        {
                            var c12c = c12.ValidateChildNode(c, "CommonItem");

                            // item name might be simple string OR array decl
                            // -- so just wrap it up as string for now
                            //var itemName = c12c.ValidateChildString(0);
                            var itemName = c12c.GetChildStringText(0, "");

                            items.Add(itemName);
                            // and check comma sep
                            c++;
                            if (c < c12cc)
                                c12.ValidateChildString(c, ",");
                        }
                        var d = new KVDict();
                        d["Name"] = blockName;
                        d["Items"] = items;
                        return new KVNode(Key, d);
                    }

                case "AssignmentStatement":
                    {
                        // shoudl have "COMMON" and object childs only
                        ValidateChildCount(3);
                        var lhs = ValidateChildNode(0, "VarRef");
                        ValidateChildString(1, "=");
                        var rhs = ValidateChildExpr(2);
                        var d = new KVDict();
                        d["lhs"] = lhs;
                        d["rhs"] = rhs;
                        return new KVNode(Key, d);
                    }

                case "VarRef":
                    {
                        string varname = ValidateChildString(0);
                        //
                        // either single vaiable nanme
                        //
                        if (cc == 1)
                        {
                            return this;
                        }
                        //
                        // or name and subscripts which we'll return in array
                        //
                        ValidateChildCount(2);
                        var ret = new KVNode(Key, varname);
                        var ss = ValidateChildNode(1, "Subscripts");
                        int sscc = ss.ChildCount;
                        int ssc = 0;
                        ss.ValidateChildString(ssc++, "(");
                        for (; ssc < sscc - 1; ssc++)
                        {
                            var sscn = ss.GetChild(ssc);
                            ret.AddChild(sscn);
                            if (ssc < sscc - 2)
                            {
                                ssc++;
                                ss.ValidateChildString(ssc, ",");
                            }
                        }
                        ss.ValidateChildString(ssc++, ")");
                        return ret;
                    }

                case "UnsignedArithmeticConstant":
                    {
                        ValidateChildCount(1);
                        var v = ValidateChildString(0);
                        if (v.EndsWith("D0"))
                        {
                            var v2 = v.Substring(0, v.Length - 2);
                            double dres;
                            if (double.TryParse(v2, out dres))
                            {
                                return new KVNodeSiblingArray(dres);
                            }
                        }
                        if (v.Contains("."))
                        {
                            double dres;
                            if (double.TryParse(v, out dres))
                            {
                                return new KVNodeSiblingArray(dres);
                            }
                        }
                        int ires;
                        if (int.TryParse(v, out ires))
                            return new KVNodeSiblingArray(ires);

                        ValidateFail("expecting number");
                        break;
                    }

                case "SubroutineStatement":
                    {
                        ValidateChildCount(6);
                        ValidateChildString(0, "SUBROUTINE");
                        var name = ValidateChildString(1);
                        ValidateChildString(2, "(");
                        var c3 = ValidateChildNode(3, "Namelist");
                        ValidateChildString(4, ")");
                        ValidateChildString(5, "\r\n");

                        var c3cc = c3.ChildCount;
                        var args = new ArrayList<object>();
                        for (int c = 0; c < c3cc; c++)
                        {
                            var argnode = c3.ValidateChildNode(c, "Identifier");
                            var argname = argnode.ValidateChildString(0);
                            if (c < c3cc - 1)
                            {
                                c++;
                                c3.ValidateChildString(c, ",");
                            }
                            args.Add(argname);
                        }

                        var ret = new KVNode(Key, null);
                        ret.AddChild(new KVNode("Name", name));
                        ret.AddChild(new KVNode("Args", args));
                        return ret;
                    }

            }
            return this;
        }

        void addExprTreeToArray(object node, List<object> output)
        {
            var kvn = node as KVNode;
            if (kvn != null)
            {
                var val = kvn.Value;
                if (kvn.Key == "expr")
                {
                    int cc = kvn.ChildCount;
                    for (int c = 0; c < cc; c++)
                    {
                        addExprTreeToArray(kvn.GetChild(c), output);
                    }
                    return;
                }
            }
            output.Add(node);
        }

        void ValidateFail(string message)
        {
            throw new Exception($"{Key} {message}");
        }

        void ValidateChildCount(int c)
        {
            if (ChildCount != c)
                ValidateFail($"expecting {c} children");
        }

        string ValidateChildString(int c, string value = null)
        {
            var c0 = GetChild(c);
            if (value == null)
            {
                if (c0 is string)
                    value = c0.ToString();
                else
                    ValidateFail($"expecting child {c} to be string value");
            }
            else
            {
                string actual = c0.ToString();
                if (actual != value)
                    ValidateFail($"expecting child {c} to be '{value}' (was {actual})");
            }
            return value;
        }

        KVNode ValidateChildNode(int c, string key = null)
        {
            KVNode c1 = GetChild(c) as KVNode;
            if (c1 == null)
                ValidateFail($"expecting child {c} to be node");
            else if (key != null && c1.Key != key)
                ValidateFail($"expecting child {c} to be {key} node (was {c1.Key})");
            return c1;
        }

        object ValidateChildExpr(int c)
        {
            object c1 = GetChild(c);
            if (c1 == null)
                ValidateFail($"expecting child {c} to be node or a value");
            var kv1 = c1 as KVNode;
            if (kv1 != null)
            {
                switch (kv1.Key)
                {
                    case "VarRef":
                        return kv1;
                    case "expr":
                        return kv1.Value;
                    default:
                        ValidateFail($"expecting expression node as VarRef or expr (was {kv1.Key}");
                        break;
                }
            }
            return c1;
        }
    }

    //
    // "dummy" KVNode to indicate its an array at sibling level when adding
    // i.e. dont add this node with its children but add its children and dump this actual node
    //
    public class KVNodeSiblingArray : KVNode
    {
        public KVNodeSiblingArray() : base("", new ArrayList<object>())
        {
        }

        public KVNodeSiblingArray(object value) : base("", value)
        {
        }
    }

    public class KVDict : OrderedDictionary
    {

    }
}
