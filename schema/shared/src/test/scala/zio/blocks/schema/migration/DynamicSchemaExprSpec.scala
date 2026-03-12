package zio.blocks.schema.migration

import zio.blocks.schema._
import zio.test._

object DynamicSchemaExprSpec extends SchemaBaseSpec {
  def spec: Spec[TestEnvironment, Any] = suite("DynamicSchemaExprSpec")(
    suite("Literal")(
      test("evaluates to constant value") {
        val expr   = DynamicSchemaExpr.int(42)
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.Int(42))))
      },
      test("ignores input") {
        val expr   = DynamicSchemaExpr.string("hello")
        val input  = DynamicValue.Record("x" -> DynamicValue.Primitive(new PrimitiveValue.Int(1)))
        val result = expr.eval(input)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.String("hello"))))
      }
    ),
    suite("Select")(
      test("selects field from record") {
        val expr  = DynamicSchemaExpr.field("name")
        val input = DynamicValue.Record("name" -> DynamicValue.Primitive(new PrimitiveValue.String("Alice")))
        val result = expr.eval(input)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.String("Alice"))))
      },
      test("selects nested field") {
        val expr  = DynamicSchemaExpr.Select(DynamicOptic.root.field("address").field("city"))
        val input = DynamicValue.Record(
          "address" -> DynamicValue.Record("city" -> DynamicValue.Primitive(new PrimitiveValue.String("NYC")))
        )
        val result = expr.eval(input)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.String("NYC"))))
      },
      test("fails for missing field") {
        val expr   = DynamicSchemaExpr.field("missing")
        val input  = DynamicValue.Record("name" -> DynamicValue.Primitive(new PrimitiveValue.String("Alice")))
        val result = expr.eval(input)
        assertTrue(result.isLeft)
      },
      test("identity returns whole input") {
        val input  = DynamicValue.Record("x" -> DynamicValue.Primitive(new PrimitiveValue.Int(1)))
        val result = DynamicSchemaExpr.identity.eval(input)
        assertTrue(result == Right(input))
      }
    ),
    suite("StringConcat")(
      test("concatenates two strings") {
        val expr = DynamicSchemaExpr.StringConcat(
          DynamicSchemaExpr.string("Hello, "),
          DynamicSchemaExpr.string("World!")
        )
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.String("Hello, World!"))))
      },
      test("concatenates field values") {
        val expr = DynamicSchemaExpr.StringConcat(
          DynamicSchemaExpr.field("first"),
          DynamicSchemaExpr.StringConcat(
            DynamicSchemaExpr.string(" "),
            DynamicSchemaExpr.field("last")
          )
        )
        val input = DynamicValue.Record(
          "first" -> DynamicValue.Primitive(new PrimitiveValue.String("John")),
          "last"  -> DynamicValue.Primitive(new PrimitiveValue.String("Doe"))
        )
        val result = expr.eval(input)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.String("John Doe"))))
      },
      test("fails on non-string operand") {
        val expr = DynamicSchemaExpr.StringConcat(
          DynamicSchemaExpr.int(42),
          DynamicSchemaExpr.string("x")
        )
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result.isLeft)
      }
    ),
    suite("StringLength")(
      test("computes string length") {
        val expr   = DynamicSchemaExpr.StringLength(DynamicSchemaExpr.string("hello"))
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.Int(5))))
      },
      test("handles empty string") {
        val expr   = DynamicSchemaExpr.StringLength(DynamicSchemaExpr.string(""))
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.Int(0))))
      }
    ),
    suite("PrimitiveConversion")(
      test("IntToLong") {
        val expr   = DynamicSchemaExpr.PrimitiveConversion(DynamicSchemaExpr.int(42), ConversionType.IntToLong)
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.Long(42L))))
      },
      test("LongToInt within range") {
        val expr = DynamicSchemaExpr.PrimitiveConversion(
          DynamicSchemaExpr.Literal(DynamicValue.Primitive(new PrimitiveValue.Long(42L))),
          ConversionType.LongToInt
        )
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.Int(42))))
      },
      test("LongToInt out of range fails") {
        val expr = DynamicSchemaExpr.PrimitiveConversion(
          DynamicSchemaExpr.Literal(DynamicValue.Primitive(new PrimitiveValue.Long(Long.MaxValue))),
          ConversionType.LongToInt
        )
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result.isLeft)
      },
      test("IntToString") {
        val expr   = DynamicSchemaExpr.PrimitiveConversion(DynamicSchemaExpr.int(42), ConversionType.IntToString)
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.String("42"))))
      },
      test("StringToInt") {
        val expr = DynamicSchemaExpr.PrimitiveConversion(DynamicSchemaExpr.string("42"), ConversionType.StringToInt)
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.Int(42))))
      },
      test("StringToInt fails on invalid input") {
        val expr = DynamicSchemaExpr.PrimitiveConversion(DynamicSchemaExpr.string("abc"), ConversionType.StringToInt)
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result.isLeft)
      },
      test("IntToDouble") {
        val expr   = DynamicSchemaExpr.PrimitiveConversion(DynamicSchemaExpr.int(42), ConversionType.IntToDouble)
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.Double(42.0))))
      },
      test("FloatToDouble") {
        val expr = DynamicSchemaExpr.PrimitiveConversion(
          DynamicSchemaExpr.Literal(DynamicValue.Primitive(new PrimitiveValue.Float(3.14f))),
          ConversionType.FloatToDouble
        )
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result.isRight)
      },
      test("BooleanToString") {
        val expr = DynamicSchemaExpr.PrimitiveConversion(DynamicSchemaExpr.boolean(true), ConversionType.BooleanToString)
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.String("true"))))
      },
      test("wrong input type fails") {
        val expr = DynamicSchemaExpr.PrimitiveConversion(DynamicSchemaExpr.string("x"), ConversionType.IntToLong)
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result.isLeft)
      }
    ),
    suite("IfThenElse")(
      test("evaluates then branch when true") {
        val expr = DynamicSchemaExpr.IfThenElse(
          DynamicSchemaExpr.boolean(true),
          DynamicSchemaExpr.string("yes"),
          DynamicSchemaExpr.string("no")
        )
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.String("yes"))))
      },
      test("evaluates else branch when false") {
        val expr = DynamicSchemaExpr.IfThenElse(
          DynamicSchemaExpr.boolean(false),
          DynamicSchemaExpr.string("yes"),
          DynamicSchemaExpr.string("no")
        )
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.String("no"))))
      },
      test("fails on non-boolean condition") {
        val expr = DynamicSchemaExpr.IfThenElse(
          DynamicSchemaExpr.int(1),
          DynamicSchemaExpr.string("yes"),
          DynamicSchemaExpr.string("no")
        )
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result.isLeft)
      }
    ),
    suite("WrapOption")(
      test("wraps value in Some variant") {
        val expr   = DynamicSchemaExpr.WrapOption(DynamicSchemaExpr.int(42))
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Variant("Some", DynamicValue.Primitive(new PrimitiveValue.Int(42)))))
      }
    ),
    suite("UnwrapOption")(
      test("unwraps Some variant") {
        val expr = DynamicSchemaExpr.UnwrapOption(
          DynamicSchemaExpr.Literal(DynamicValue.Variant("Some", DynamicValue.Primitive(new PrimitiveValue.Int(42)))),
          DynamicSchemaExpr.int(0)
        )
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.Int(42))))
      },
      test("returns default for None variant") {
        val expr = DynamicSchemaExpr.UnwrapOption(
          DynamicSchemaExpr.Literal(DynamicValue.Variant("None", DynamicValue.Null)),
          DynamicSchemaExpr.int(99)
        )
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.Int(99))))
      },
      test("returns default for Null") {
        val expr = DynamicSchemaExpr.UnwrapOption(DynamicSchemaExpr.nil, DynamicSchemaExpr.int(99))
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.Int(99))))
      }
    ),
    suite("ConstructRecord")(
      test("constructs record from expressions") {
        val expr = DynamicSchemaExpr.ConstructRecord(Vector(
          "name" -> DynamicSchemaExpr.string("Alice"),
          "age"  -> DynamicSchemaExpr.int(30)
        ))
        val result = expr.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Record(
          "name" -> DynamicValue.Primitive(new PrimitiveValue.String("Alice")),
          "age"  -> DynamicValue.Primitive(new PrimitiveValue.Int(30))
        )))
      },
      test("constructs record using source fields") {
        val expr = DynamicSchemaExpr.ConstructRecord(Vector(
          "greeting" -> DynamicSchemaExpr.StringConcat(
            DynamicSchemaExpr.string("Hello, "),
            DynamicSchemaExpr.field("name")
          )
        ))
        val input = DynamicValue.Record("name" -> DynamicValue.Primitive(new PrimitiveValue.String("Bob")))
        val result = expr.eval(input)
        assertTrue(result == Right(DynamicValue.Record(
          "greeting" -> DynamicValue.Primitive(new PrimitiveValue.String("Hello, Bob"))
        )))
      }
    ),
    suite("smart constructors")(
      test("nil produces Null") {
        val result = DynamicSchemaExpr.nil.eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Null))
      },
      test("long produces Long primitive") {
        val result = DynamicSchemaExpr.long(100L).eval(DynamicValue.Null)
        assertTrue(result == Right(DynamicValue.Primitive(new PrimitiveValue.Long(100L))))
      }
    )
  )
}
