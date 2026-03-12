package zio.blocks.schema.migration

import zio.blocks.schema._
import zio.test._

/**
 * End-to-end integration tests for the schema migration system, testing
 * realistic migration scenarios across schema versions.
 */
object MigrationIntegrationSpec extends SchemaBaseSpec {
  private def record(fields: (String, DynamicValue)*): DynamicValue = DynamicValue.Record(fields: _*)
  private def str(s: String): DynamicValue                         = DynamicValue.Primitive(new PrimitiveValue.String(s))
  private def int(i: Int): DynamicValue                            = DynamicValue.Primitive(new PrimitiveValue.Int(i))
  private def long(l: Long): DynamicValue                          = DynamicValue.Primitive(new PrimitiveValue.Long(l))

  def spec: Spec[TestEnvironment, Any] = suite("MigrationIntegrationSpec")(
    suite("multi-step schema evolution")(
      test("V1 -> V2: rename + add field") {
        // V1: { firstName: String }
        // V2: { name: String, age: Int }
        val migration = new DynamicMigration(Vector(
          MigrationAction.Rename(DynamicOptic.root, "firstName", "name"),
          MigrationAction.AddField(DynamicOptic.root, "age", DynamicSchemaExpr.int(0))
        ))
        val v1     = record("firstName" -> str("Alice"))
        val result = migration(v1)
        assertTrue(result == Right(record("name" -> str("Alice"), "age" -> int(0))))
      },
      test("V2 -> V3: drop field + change type") {
        // V2: { name: String, age: Int, legacy: String }
        // V3: { name: String, age: Long }
        val migration = new DynamicMigration(Vector(
          MigrationAction.DropField(DynamicOptic.root, "legacy", DynamicSchemaExpr.string("")),
          MigrationAction.ChangeType(
            DynamicOptic.root, "age",
            DynamicSchemaExpr.PrimitiveConversion(DynamicSchemaExpr.field("age"), ConversionType.IntToLong)
          )
        ))
        val v2     = record("name" -> str("Alice"), "age" -> int(30), "legacy" -> str("old"))
        val result = migration(v2)
        assertTrue(result == Right(record("name" -> str("Alice"), "age" -> long(30L))))
      },
      test("chained V1 -> V2 -> V3") {
        val v1ToV2 = new DynamicMigration(Vector(
          MigrationAction.Rename(DynamicOptic.root, "firstName", "name"),
          MigrationAction.AddField(DynamicOptic.root, "age", DynamicSchemaExpr.int(0))
        ))
        val v2ToV3 = new DynamicMigration(Vector(
          MigrationAction.ChangeType(
            DynamicOptic.root, "age",
            DynamicSchemaExpr.PrimitiveConversion(DynamicSchemaExpr.field("age"), ConversionType.IntToLong)
          )
        ))
        val v1ToV3 = v1ToV2 ++ v2ToV3
        val v1     = record("firstName" -> str("Alice"))
        val result = v1ToV3(v1)
        assertTrue(result == Right(record("name" -> str("Alice"), "age" -> long(0L))))
      }
    ),
    suite("nested schema migration")(
      test("migrate nested address record") {
        val addressMigration = new DynamicMigration(Vector(
          MigrationAction.Rename(DynamicOptic.root, "street", "streetName"),
          MigrationAction.AddField(DynamicOptic.root, "country", DynamicSchemaExpr.string("US"))
        ))
        val migration = DynamicMigration(
          MigrationAction.ApplyMigration(DynamicOptic.root.field("address"), addressMigration)
        )
        val input = record(
          "name"    -> str("Alice"),
          "address" -> record("street" -> str("123 Main"), "city" -> str("NYC"))
        )
        val result = migration(input)
        assertTrue(result == Right(record(
          "name"    -> str("Alice"),
          "address" -> record(
            "streetName" -> str("123 Main"),
            "city"       -> str("NYC"),
            "country"    -> str("US")
          )
        )))
      },
      test("migrate deeply nested structure") {
        val innerMigration = DynamicMigration(MigrationAction.Rename(DynamicOptic.root, "x", "value"))
        val midMigration = DynamicMigration(
          MigrationAction.ApplyMigration(DynamicOptic.root.field("inner"), innerMigration)
        )
        val outerMigration = DynamicMigration(
          MigrationAction.ApplyMigration(DynamicOptic.root.field("mid"), midMigration)
        )
        val input = record(
          "mid" -> record(
            "inner" -> record("x" -> int(42))
          )
        )
        val result = outerMigration(input)
        assertTrue(result == Right(record(
          "mid" -> record(
            "inner" -> record("value" -> int(42))
          )
        )))
      }
    ),
    suite("enum/variant migration")(
      test("rename enum case") {
        val migration = DynamicMigration(
          MigrationAction.RenameCase(DynamicOptic.root, "Active", "Enabled")
        )
        val input  = DynamicValue.Variant("Active", record("since" -> str("2024-01-01")))
        val result = migration(input)
        assertTrue(result == Right(DynamicValue.Variant("Enabled", record("since" -> str("2024-01-01")))))
      },
      test("transform enum case contents") {
        val caseMigration = new DynamicMigration(Vector(
          MigrationAction.AddField(DynamicOptic.root, "reason", DynamicSchemaExpr.string("unknown"))
        ))
        val migration = DynamicMigration(
          MigrationAction.TransformCase(DynamicOptic.root, "Error", caseMigration)
        )
        val input  = DynamicValue.Variant("Error", record("code" -> int(404)))
        val result = migration(input)
        assertTrue(result == Right(DynamicValue.Variant("Error", record(
          "code"   -> int(404),
          "reason" -> str("unknown")
        ))))
      },
      test("rename case then transform combined") {
        val migration = new DynamicMigration(Vector(
          MigrationAction.RenameCase(DynamicOptic.root, "Err", "Error"),
          MigrationAction.TransformCase(
            DynamicOptic.root, "Error",
            new DynamicMigration(Vector(
              MigrationAction.AddField(DynamicOptic.root, "message", DynamicSchemaExpr.string(""))
            ))
          )
        ))
        val input  = DynamicValue.Variant("Err", DynamicValue.Record.empty)
        val result = migration(input)
        assertTrue(result == Right(DynamicValue.Variant("Error", record("message" -> str("")))))
      }
    ),
    suite("collection migration")(
      test("transform all list elements") {
        val migration = DynamicMigration(MigrationAction.TransformElements(
          DynamicOptic.root.field("scores"),
          DynamicSchemaExpr.PrimitiveConversion(DynamicSchemaExpr.identity, ConversionType.IntToLong)
        ))
        val input = record(
          "scores" -> DynamicValue.Sequence(zio.blocks.chunk.Chunk(int(90), int(85), int(95)))
        )
        val result = migration(input)
        assertTrue(result == Right(record(
          "scores" -> DynamicValue.Sequence(zio.blocks.chunk.Chunk(long(90L), long(85L), long(95L)))
        )))
      },
      test("migrate records inside a sequence") {
        val migration = DynamicMigration(MigrationAction.AddField(
          DynamicOptic.root.field("items").elements,
          "active",
          DynamicSchemaExpr.boolean(true)
        ))
        val input = record(
          "items" -> DynamicValue.Sequence(zio.blocks.chunk.Chunk(
            record("name" -> str("Item1")),
            record("name" -> str("Item2"))
          ))
        )
        val result = migration(input)
        assertTrue(result.isRight)
        val items = result.toOption.get match {
          case r: DynamicValue.Record => r.fields(0)._2 match {
            case s: DynamicValue.Sequence => s.elements
            case _ => zio.blocks.chunk.Chunk.empty[DynamicValue]
          }
          case _ => zio.blocks.chunk.Chunk.empty[DynamicValue]
        }
        assertTrue(items.length == 2)
      },
      test("transform map keys") {
        val migration = DynamicMigration(MigrationAction.TransformKeys(
          DynamicOptic.root,
          DynamicSchemaExpr.PrimitiveConversion(DynamicSchemaExpr.identity, ConversionType.IntToString)
        ))
        val input = DynamicValue.Map(zio.blocks.chunk.Chunk(
          (int(1), str("one")),
          (int(2), str("two"))
        ))
        val result = migration(input)
        assertTrue(result == Right(DynamicValue.Map(zio.blocks.chunk.Chunk(
          (str("1"), str("one")),
          (str("2"), str("two"))
        ))))
      }
    ),
    suite("option field migration")(
      test("mandate optional field") {
        val migration = DynamicMigration(MigrationAction.Mandate(
          DynamicOptic.root, "email", DynamicSchemaExpr.string("no-email")
        ))
        val withEmail = record("email" -> DynamicValue.Variant("Some", str("a@b.com")))
        val withNone  = record("email" -> DynamicValue.Variant("None", DynamicValue.Null))

        val r1 = migration(withEmail)
        val r2 = migration(withNone)
        assertTrue(r1 == Right(record("email" -> str("a@b.com"))))
        assertTrue(r2 == Right(record("email" -> str("no-email"))))
      },
      test("optionalize then mandate roundtrip") {
        val optionalize = DynamicMigration(MigrationAction.Optionalize(DynamicOptic.root, "name"))
        val mandate     = DynamicMigration(MigrationAction.Mandate(DynamicOptic.root, "name", DynamicSchemaExpr.nil))

        val input    = record("name" -> str("Alice"))
        val optional = optionalize(input)
        assertTrue(optional.isRight)
        val mandated = mandate(optional.toOption.get)
        assertTrue(mandated == Right(input))
      }
    ),
    suite("optimization integration")(
      test("optimized migration produces same result as unoptimized") {
        val m = new DynamicMigration(Vector(
          MigrationAction.Rename(DynamicOptic.root, "a", "b"),
          MigrationAction.Rename(DynamicOptic.root, "b", "c"),
          MigrationAction.AddField(DynamicOptic.root, "d", DynamicSchemaExpr.int(0))
        ))
        val input     = record("a" -> int(1))
        val original  = m(input)
        val optimized = m.optimize(input)
        assertTrue(original == optimized)
        assertTrue(m.optimize.actions.length < m.actions.length)
      }
    ),
    suite("error handling")(
      test("errors include path information") {
        val migration = DynamicMigration(MigrationAction.Rename(
          DynamicOptic.root.field("address"), "missing", "target"
        ))
        val input = record("address" -> record("city" -> str("NYC")))
        val result = migration(input)
        assertTrue(result.isLeft)
        val error = result.swap.toOption.get
        assertTrue(error.message.contains("missing") || error.message.contains("not found"))
      },
      test("chained migration reports first failure") {
        val migration = new DynamicMigration(Vector(
          MigrationAction.Rename(DynamicOptic.root, "x", "y"),    // succeeds
          MigrationAction.DropField(DynamicOptic.root, "z", DynamicSchemaExpr.nil) // fails: z doesn't exist
        ))
        val input  = record("x" -> int(1))
        val result = migration(input)
        assertTrue(result.isLeft)
      }
    ),
    suite("typed Migration")(
      test("typed identity migration preserves value") {
        case class Person(name: String, age: Int)
        implicit val schema: Schema[Person] = Schema.derived
        val m = Migration.identity[Person]
        val result = m(Person("Alice", 30))
        assertTrue(result == Right(Person("Alice", 30)))
      },
      test("typed migration composition") {
        case class V1(x: Int)
        case class V2(x: Int, y: Int)
        case class V3(x: Int, y: Int, z: Int)
        implicit val s1: Schema[V1] = Schema.derived
        implicit val s2: Schema[V2] = Schema.derived
        implicit val s3: Schema[V3] = Schema.derived

        val m1 = Migration.builder[V1, V2]
          .addField("y", DynamicSchemaExpr.int(0))
          .buildPartial

        val m2 = Migration.builder[V2, V3]
          .addField("z", DynamicSchemaExpr.int(0))
          .buildPartial

        val composed = m1 ++ m2
        val result   = composed(V1(42))
        assertTrue(result == Right(V3(42, 0, 0)))
      },
      test("typed migration reverse") {
        case class V1(a: String)
        case class V2(b: String)
        implicit val s1: Schema[V1] = Schema.derived
        implicit val s2: Schema[V2] = Schema.derived

        val m = Migration.builder[V1, V2]
          .renameField("a", "b")
          .buildPartial

        val forward  = m(V1("hello"))
        assertTrue(forward == Right(V2("hello")))
        val backward = m.reverse(V2("hello"))
        assertTrue(backward == Right(V1("hello")))
      }
    ),
    suite("MigrationBuilder")(
      test("buildPartial creates migration without validation") {
        case class A(x: Int)
        case class B(y: String)
        implicit val sa: Schema[A] = Schema.derived
        implicit val sb: Schema[B] = Schema.derived

        // This is intentionally incomplete but buildPartial should succeed
        val m = Migration.builder[A, B]
          .renameField("x", "y")
          .buildPartial

        assertTrue(m.size == 1)
      },
      test("builder is immutable") {
        case class A(x: Int)
        case class B(x: Int, y: Int)
        implicit val sa: Schema[A] = Schema.derived
        implicit val sb: Schema[B] = Schema.derived

        val b1 = Migration.builder[A, B]
        val b2 = b1.addField("y", DynamicSchemaExpr.int(0))
        assertTrue(b1.actions.isEmpty)
        assertTrue(b2.actions.length == 1)
      },
      test("build validates against schemas") {
        case class A(x: Int)
        case class B(x: Int, y: Int)
        implicit val sa: Schema[A] = Schema.derived
        implicit val sb: Schema[B] = Schema.derived

        val valid = Migration.builder[A, B]
          .addField("y", DynamicSchemaExpr.int(0))
          .build

        assertTrue(valid.isRight)
      },
      test("build fails when target field unaccounted") {
        case class A(x: Int)
        case class B(x: Int, y: Int)
        implicit val sa: Schema[A] = Schema.derived
        implicit val sb: Schema[B] = Schema.derived

        val invalid = Migration.builder[A, B].build
        assertTrue(invalid.isLeft)
      },
      test("build fails when source field unhandled") {
        case class A(x: Int, y: Int)
        case class B(x: Int)
        implicit val sa: Schema[A] = Schema.derived
        implicit val sb: Schema[B] = Schema.derived

        val invalid = Migration.builder[A, B].build
        assertTrue(invalid.isLeft)
      },
      test("build succeeds with rename covering field movement") {
        case class A(oldName: String)
        case class B(newName: String)
        implicit val sa: Schema[A] = Schema.derived
        implicit val sb: Schema[B] = Schema.derived

        val valid = Migration.builder[A, B]
          .renameField("oldName", "newName")
          .build

        assertTrue(valid.isRight)
      },
      test("build succeeds with drop covering removed field") {
        case class A(x: Int, y: Int)
        case class B(x: Int)
        implicit val sa: Schema[A] = Schema.derived
        implicit val sb: Schema[B] = Schema.derived

        val valid = Migration.builder[A, B]
          .dropField("y")
          .build

        assertTrue(valid.isRight)
      },
      test("nested operations via addFieldAt") {
        case class Inner(x: Int)
        case class Outer(inner: Inner)
        case class OuterV2(inner: Inner) // same shape, but we add to inner
        implicit val si: Schema[Inner]   = Schema.derived
        implicit val so: Schema[Outer]   = Schema.derived
        implicit val so2: Schema[OuterV2] = Schema.derived

        val m = Migration.builder[Outer, OuterV2]
          .addFieldAt(DynamicOptic.root.field("inner"), "y", DynamicSchemaExpr.int(0))
          .buildPartial

        val result = m(Outer(Inner(42)))
        assertTrue(result.isLeft) // Will fail at fromDynamicValue because OuterV2.inner doesn't have y
        // But the dynamic migration itself should succeed
        val dynResult = m.dynamicMigration(Schema[Outer].toDynamicValue(Outer(Inner(42))))
        assertTrue(dynResult.isRight)
      }
    )
  )
}
