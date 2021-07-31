using Antlr4.Runtime.Misc;
using System;
using System.Diagnostics;
using System.Text.Json;
using System.Text.Json.Serialization;

namespace antlr4_fortran_parser
{
    public class KVNodeConverter : JsonConverter<KVNode>
    {
        public override KVNode Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
        {
            throw new NotImplementedException();
        }

        public override void Write(Utf8JsonWriter writer, KVNode value, JsonSerializerOptions options)
        {
            writer.WriteStartObject();
            writer.WritePropertyName(value.Key);
            WriteValue(writer, value.Value, options);
            writer.WriteEndObject();
        }

        public void WriteValue(Utf8JsonWriter writer, object value, JsonSerializerOptions options)
        {
            if (value == null)
            {
                writer.WriteNullValue();
            }
            else if (value is KVNode)
            {
                Write(writer, value as KVNode, options);
            }
            else if (value is ArrayList<object>)
            {
                var a = value as ArrayList<object>;
                writer.WriteStartArray();
                foreach (var av in a)
                {
                    WriteValue(writer, av, options);
                }
                writer.WriteEndArray();
            }
            else if (value is string)
            {
                writer.WriteStringValue(value.ToString());
            }
            else
                Debug.Fail("Unexpected value in KVNode");
        }
    }

}
