/*
 * Copyright 2024-2026 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.blocks.schema.migration

import zio.blocks.schema._

/**
 * A pure, serializable expression language for use in schema migrations.
 *
 * Unlike [[SchemaExpr]], which is typed and uses Scala functions,
 * `DynamicSchemaExpr` operates entirely on [[DynamicValue]] and contains no
 * closures. This makes it fully serializable and suitable for storage in
 * migration registries, transmission over the wire, and offline application.
 *
 * Expressions can reference the source document via [[Select]] paths, produce
 * constant values via [[Literal]], perform string operations, and convert
 * between primitive types.
 */
sealed trait DynamicSchemaExpr {

  /**
   * Evaluates this expression against a source [[DynamicValue]].
   *
   * @param input
   *   the source value to evaluate against
   * @return
   *   either a [[SchemaError]] describing the failure, or the resulting value
   */
  def eval(input: DynamicValue): Either[SchemaError, DynamicValue]
}

object DynamicSchemaExpr {

  /**
   * A constant literal value.
   *
   * @param value
   *   the constant value to produce
   */
  final case class Literal(value: DynamicValue) extends DynamicSchemaExpr {
    def eval(input: DynamicValue): Either[SchemaError, DynamicValue] = Right(value)
  }

  /**
   * Selects a value from the source document at the given path.
   *
   * @param path
   *   the optic path to navigate in the source
   */
  final case class Select(path: DynamicOptic) extends DynamicSchemaExpr {
    def eval(input: DynamicValue): Either[SchemaError, DynamicValue] =
      input.get(path).one
  }

  /**
   * Concatenates the string representations of two expressions.
   */
  final case class StringConcat(left: DynamicSchemaExpr, right: DynamicSchemaExpr) extends DynamicSchemaExpr {
    def eval(input: DynamicValue): Either[SchemaError, DynamicValue] =
      for {
        l <- left.eval(input)
        r <- right.eval(input)
        ls <- extractString(l, "StringConcat left operand")
        rs <- extractString(r, "StringConcat right operand")
      } yield DynamicValue.Primitive(new PrimitiveValue.String(ls + rs))
  }

  /**
   * Computes the length of a string expression.
   */
  final case class StringLength(expr: DynamicSchemaExpr) extends DynamicSchemaExpr {
    def eval(input: DynamicValue): Either[SchemaError, DynamicValue] =
      for {
        v <- expr.eval(input)
        s <- extractString(v, "StringLength operand")
      } yield DynamicValue.Primitive(new PrimitiveValue.Int(s.length))
  }

  /**
   * Converts between primitive types.
   *
   * Supported conversions include numeric widening (Int -> Long, Float ->
   * Double, etc.), numeric narrowing with bounds checking, and string
   * conversions (toString / parse).
   */
  final case class PrimitiveConversion(expr: DynamicSchemaExpr, conversion: ConversionType) extends DynamicSchemaExpr {
    def eval(input: DynamicValue): Either[SchemaError, DynamicValue] =
      for {
        v      <- expr.eval(input)
        result <- conversion.convert(v)
      } yield result
  }

  /**
   * Conditional expression: if condition then thenExpr else elseExpr.
   */
  final case class IfThenElse(
    condition: DynamicSchemaExpr,
    thenExpr: DynamicSchemaExpr,
    elseExpr: DynamicSchemaExpr
  ) extends DynamicSchemaExpr {
    def eval(input: DynamicValue): Either[SchemaError, DynamicValue] =
      for {
        cond <- condition.eval(input)
        b    <- extractBoolean(cond, "IfThenElse condition")
        result <- if (b) thenExpr.eval(input) else elseExpr.eval(input)
      } yield result
  }

  /**
   * Wraps a value in Option (Some).
   */
  final case class WrapOption(expr: DynamicSchemaExpr) extends DynamicSchemaExpr {
    def eval(input: DynamicValue): Either[SchemaError, DynamicValue] =
      for {
        v <- expr.eval(input)
      } yield DynamicValue.Variant("Some", v)
  }

  /**
   * Unwraps an Option value, providing a default if None/Null.
   */
  final case class UnwrapOption(expr: DynamicSchemaExpr, default: DynamicSchemaExpr) extends DynamicSchemaExpr {
    def eval(input: DynamicValue): Either[SchemaError, DynamicValue] =
      for {
        v <- expr.eval(input)
        result <- v match {
                    case variant: DynamicValue.Variant if variant.caseNameValue == "Some" => Right(variant.value)
                    case variant: DynamicValue.Variant if variant.caseNameValue == "None" => default.eval(input)
                    case _: DynamicValue.Null.type                              => default.eval(input)
                    case other                                                  => Right(other)
                  }
      } yield result
  }

  /**
   * Constructs a record from a list of field name/expression pairs.
   */
  final case class ConstructRecord(fields: Vector[(String, DynamicSchemaExpr)]) extends DynamicSchemaExpr {
    def eval(input: DynamicValue): Either[SchemaError, DynamicValue] = {
      var result  = Vector.empty[(String, DynamicValue)]
      val len     = fields.length
      var idx     = 0
      while (idx < len) {
        val (name, expr) = fields(idx)
        expr.eval(input) match {
          case Right(v) => result = result :+ (name -> v)
          case Left(e)  => return Left(e)
        }
        idx += 1
      }
      Right(DynamicValue.Record(result: _*))
    }
  }

  // ── Helpers ──────────────────────────────────────────────────────────────

  private def extractString(dv: DynamicValue, context: String): Either[SchemaError, String] =
    dv match {
      case p: DynamicValue.Primitive =>
        p.value match {
          case s: PrimitiveValue.String => Right(s.value)
          case _                        => Left(SchemaError(s"$context: expected String, got ${p.value.getClass.getSimpleName}"))
        }
      case _ => Left(SchemaError(s"$context: expected Primitive(String), got ${dv.valueType}"))
    }

  private def extractBoolean(dv: DynamicValue, context: String): Either[SchemaError, Boolean] =
    dv match {
      case p: DynamicValue.Primitive =>
        p.value match {
          case b: PrimitiveValue.Boolean => Right(b.value)
          case _                         => Left(SchemaError(s"$context: expected Boolean, got ${p.value.getClass.getSimpleName}"))
        }
      case _ => Left(SchemaError(s"$context: expected Primitive(Boolean), got ${dv.valueType}"))
    }

  // ── Smart constructors ──────────────────────────────────────────────────

  /** Creates a literal string expression. */
  def string(value: String): DynamicSchemaExpr =
    Literal(DynamicValue.Primitive(new PrimitiveValue.String(value)))

  /** Creates a literal integer expression. */
  def int(value: Int): DynamicSchemaExpr =
    Literal(DynamicValue.Primitive(new PrimitiveValue.Int(value)))

  /** Creates a literal long expression. */
  def long(value: Long): DynamicSchemaExpr =
    Literal(DynamicValue.Primitive(new PrimitiveValue.Long(value)))

  /** Creates a literal boolean expression. */
  def boolean(value: Boolean): DynamicSchemaExpr =
    Literal(DynamicValue.Primitive(new PrimitiveValue.Boolean(value)))

  /** Creates an expression that selects a field from the source. */
  def field(name: String): DynamicSchemaExpr =
    Select(DynamicOptic.root.field(name))

  /** Creates an expression that returns the entire source document. */
  val identity: DynamicSchemaExpr = Select(DynamicOptic.root)

  /** Creates a Null literal. */
  val nil: DynamicSchemaExpr = Literal(DynamicValue.Null)
}

/**
 * Represents a type conversion between primitive types.
 *
 * All conversions are pure data and fully serializable.
 */
sealed trait ConversionType {
  def convert(value: DynamicValue): Either[SchemaError, DynamicValue]
}

object ConversionType {

  case object IntToLong extends ConversionType {
    def convert(value: DynamicValue): Either[SchemaError, DynamicValue] = value match {
      case p: DynamicValue.Primitive =>
        p.value match {
          case v: PrimitiveValue.Int => Right(DynamicValue.Primitive(new PrimitiveValue.Long(v.value.toLong)))
          case _                     => Left(SchemaError(s"IntToLong: expected Int, got ${p.value.getClass.getSimpleName}"))
        }
      case _ => Left(SchemaError("IntToLong: expected Primitive value"))
    }
  }

  case object LongToInt extends ConversionType {
    def convert(value: DynamicValue): Either[SchemaError, DynamicValue] = value match {
      case p: DynamicValue.Primitive =>
        p.value match {
          case v: PrimitiveValue.Long =>
            val l = v.value
            if (l >= Int.MinValue && l <= Int.MaxValue)
              Right(DynamicValue.Primitive(new PrimitiveValue.Int(l.toInt)))
            else
              Left(SchemaError(s"LongToInt: value $l out of Int range"))
          case _ => Left(SchemaError(s"LongToInt: expected Long, got ${p.value.getClass.getSimpleName}"))
        }
      case _ => Left(SchemaError("LongToInt: expected Primitive value"))
    }
  }

  case object IntToDouble extends ConversionType {
    def convert(value: DynamicValue): Either[SchemaError, DynamicValue] = value match {
      case p: DynamicValue.Primitive =>
        p.value match {
          case v: PrimitiveValue.Int => Right(DynamicValue.Primitive(new PrimitiveValue.Double(v.value.toDouble)))
          case _                     => Left(SchemaError(s"IntToDouble: expected Int, got ${p.value.getClass.getSimpleName}"))
        }
      case _ => Left(SchemaError("IntToDouble: expected Primitive value"))
    }
  }

  case object IntToString extends ConversionType {
    def convert(value: DynamicValue): Either[SchemaError, DynamicValue] = value match {
      case p: DynamicValue.Primitive =>
        p.value match {
          case v: PrimitiveValue.Int => Right(DynamicValue.Primitive(new PrimitiveValue.String(v.value.toString)))
          case _                     => Left(SchemaError(s"IntToString: expected Int, got ${p.value.getClass.getSimpleName}"))
        }
      case _ => Left(SchemaError("IntToString: expected Primitive value"))
    }
  }

  case object StringToInt extends ConversionType {
    def convert(value: DynamicValue): Either[SchemaError, DynamicValue] = value match {
      case p: DynamicValue.Primitive =>
        p.value match {
          case v: PrimitiveValue.String =>
            try Right(DynamicValue.Primitive(new PrimitiveValue.Int(v.value.toInt)))
            catch { case _: NumberFormatException => Left(SchemaError(s"StringToInt: cannot parse '${v.value}'")) }
          case _ => Left(SchemaError(s"StringToInt: expected String, got ${p.value.getClass.getSimpleName}"))
        }
      case _ => Left(SchemaError("StringToInt: expected Primitive value"))
    }
  }

  case object LongToString extends ConversionType {
    def convert(value: DynamicValue): Either[SchemaError, DynamicValue] = value match {
      case p: DynamicValue.Primitive =>
        p.value match {
          case v: PrimitiveValue.Long => Right(DynamicValue.Primitive(new PrimitiveValue.String(v.value.toString)))
          case _                      => Left(SchemaError(s"LongToString: expected Long, got ${p.value.getClass.getSimpleName}"))
        }
      case _ => Left(SchemaError("LongToString: expected Primitive value"))
    }
  }

  case object StringToLong extends ConversionType {
    def convert(value: DynamicValue): Either[SchemaError, DynamicValue] = value match {
      case p: DynamicValue.Primitive =>
        p.value match {
          case v: PrimitiveValue.String =>
            try Right(DynamicValue.Primitive(new PrimitiveValue.Long(v.value.toLong)))
            catch { case _: NumberFormatException => Left(SchemaError(s"StringToLong: cannot parse '${v.value}'")) }
          case _ => Left(SchemaError(s"StringToLong: expected String, got ${p.value.getClass.getSimpleName}"))
        }
      case _ => Left(SchemaError("StringToLong: expected Primitive value"))
    }
  }

  case object DoubleToString extends ConversionType {
    def convert(value: DynamicValue): Either[SchemaError, DynamicValue] = value match {
      case p: DynamicValue.Primitive =>
        p.value match {
          case v: PrimitiveValue.Double => Right(DynamicValue.Primitive(new PrimitiveValue.String(v.value.toString)))
          case _                        => Left(SchemaError(s"DoubleToString: expected Double, got ${p.value.getClass.getSimpleName}"))
        }
      case _ => Left(SchemaError("DoubleToString: expected Primitive value"))
    }
  }

  case object FloatToDouble extends ConversionType {
    def convert(value: DynamicValue): Either[SchemaError, DynamicValue] = value match {
      case p: DynamicValue.Primitive =>
        p.value match {
          case v: PrimitiveValue.Float => Right(DynamicValue.Primitive(new PrimitiveValue.Double(v.value.toDouble)))
          case _                       => Left(SchemaError(s"FloatToDouble: expected Float, got ${p.value.getClass.getSimpleName}"))
        }
      case _ => Left(SchemaError("FloatToDouble: expected Primitive value"))
    }
  }

  case object BooleanToString extends ConversionType {
    def convert(value: DynamicValue): Either[SchemaError, DynamicValue] = value match {
      case p: DynamicValue.Primitive =>
        p.value match {
          case v: PrimitiveValue.Boolean => Right(DynamicValue.Primitive(new PrimitiveValue.String(v.value.toString)))
          case _                         => Left(SchemaError(s"BooleanToString: expected Boolean, got ${p.value.getClass.getSimpleName}"))
        }
      case _ => Left(SchemaError("BooleanToString: expected Primitive value"))
    }
  }
}
