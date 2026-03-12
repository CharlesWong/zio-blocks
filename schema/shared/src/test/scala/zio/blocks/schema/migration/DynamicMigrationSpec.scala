package zio.blocks.schema.migration

import zio.blocks.schema._
import zio.test._

object DynamicMigrationSpec extends SchemaBaseSpec {
  private def record(fields: (String, DynamicValue)*): DynamicValue = DynamicValue.Record(fields: _*)
  private def str(s: String): DynamicValue                         = DynamicValue.Primitive(new PrimitiveValue.String(s))
  private def int(i: Int): DynamicValue                            = DynamicValue.Primitive(new PrimitiveValue.Int(i))
  private def long(l: Long): DynamicValue                          = DynamicValue.Primitive(new PrimitiveValue.Long(l))

  def spec: Spec[TestEnvironment, Any] = suite("DynamicMigrationSpec")(
    suite("AddField")(
      test("adds field to empty record") {
        val migration = DynamicMigration(MigrationAction.AddField(
          DynamicOptic.root, "name", DynamicSchemaExpr.string("default")
        ))
        val result = migration(DynamicValue.Record.empty)
        assertTrue(result == Right(record("name" -> str("default"))))
      },
      test("adds field to existing record") {
        val migration = DynamicMigration(MigrationAction.AddField(
          DynamicOptic.root, "age", DynamicSchemaExpr.int(0)
        ))
        val input  = record("name" -> str("Alice"))
        val result = migration(input)
        assertTrue(result == Right(record("name" -> str("Alice"), "age" -> int(0))))
      },
      test("fails when field already exists") {
        val migration = DynamicMigration(MigrationAction.AddField(
          DynamicOptic.root, "name", DynamicSchemaExpr.string("default")
        ))
        val input  = record("name" -> str("Alice"))
        val result = migration(input)
        assertTrue(result.isLeft)
      },
      test("fails on non-record") {
        val migration = DynamicMigration(MigrationAction.AddField(
          DynamicOptic.root, "x", DynamicSchemaExpr.int(0)
        ))
        val result = migration(str("not a record"))
        assertTrue(result.isLeft)
      }
    ),
    suite("DropField")(
      test("drops existing field") {
        val migration = DynamicMigration(MigrationAction.DropField(
          DynamicOptic.root, "age", DynamicSchemaExpr.int(0)
        ))
        val input  = record("name" -> str("Alice"), "age" -> int(30))
        val result = migration(input)
        assertTrue(result == Right(record("name" -> str("Alice"))))
      },
      test("drops only field leaves empty record") {
        val migration = DynamicMigration(MigrationAction.DropField(
          DynamicOptic.root, "x", DynamicSchemaExpr.int(0)
        ))
        val input  = record("x" -> int(1))
        val result = migration(input)
        assertTrue(result == Right(DynamicValue.Record.empty))
      },
      test("fails when field not found") {
        val migration = DynamicMigration(MigrationAction.DropField(
          DynamicOptic.root, "missing", DynamicSchemaExpr.nil
        ))
        val result = migration(record("name" -> str("Alice")))
        assertTrue(result.isLeft)
      }
    ),
    suite("Rename")(
      test("renames field") {
        val migration = DynamicMigration(MigrationAction.Rename(
          DynamicOptic.root, "firstName", "name"
        ))
        val input  = record("firstName" -> str("Alice"))
        val result = migration(input)
        assertTrue(result == Right(record("name" -> str("Alice"))))
      },
      test("preserves other fields") {
        val migration = DynamicMigration(MigrationAction.Rename(
          DynamicOptic.root, "x", "y"
        ))
        val input  = record("x" -> int(1), "z" -> int(2))
        val result = migration(input)
        assertTrue(result == Right(record("y" -> int(1), "z" -> int(2))))
      },
      test("fails when source not found") {
        val migration = DynamicMigration(MigrationAction.Rename(
          DynamicOptic.root, "missing", "target"
        ))
        val result = migration(record("other" -> int(1)))
        assertTrue(result.isLeft)
      },
      test("fails when target already exists") {
        val migration = DynamicMigration(MigrationAction.Rename(
          DynamicOptic.root, "x", "y"
        ))
        val result = migration(record("x" -> int(1), "y" -> int(2)))
        assertTrue(result.isLeft)
      }
    ),
    suite("TransformValue")(
      test("transforms field using expression") {
        val migration = DynamicMigration(MigrationAction.TransformValue(
          DynamicOptic.root, "age",
          DynamicSchemaExpr.PrimitiveConversion(
            DynamicSchemaExpr.field("age"),
            ConversionType.IntToLong
          )
        ))
        val input  = record("age" -> int(30))
        val result = migration(input)
        assertTrue(result == Right(record("age" -> long(30L))))
      },
      test("fails when field not found") {
        val migration = DynamicMigration(MigrationAction.TransformValue(
          DynamicOptic.root, "missing", DynamicSchemaExpr.int(0)
        ))
        val result = migration(record("x" -> int(1)))
        assertTrue(result.isLeft)
      }
    ),
    suite("Mandate")(
      test("unwraps Some option") {
        val migration = DynamicMigration(MigrationAction.Mandate(
          DynamicOptic.root, "email", DynamicSchemaExpr.string("none@example.com")
        ))
        val input  = record("email" -> DynamicValue.Variant("Some", str("a@b.com")))
        val result = migration(input)
        assertTrue(result == Right(record("email" -> str("a@b.com"))))
      },
      test("uses default for None option") {
        val migration = DynamicMigration(MigrationAction.Mandate(
          DynamicOptic.root, "email", DynamicSchemaExpr.string("none@example.com")
        ))
        val input  = record("email" -> DynamicValue.Variant("None", DynamicValue.Null))
        val result = migration(input)
        assertTrue(result == Right(record("email" -> str("none@example.com"))))
      },
      test("uses default for Null") {
        val migration = DynamicMigration(MigrationAction.Mandate(
          DynamicOptic.root, "email", DynamicSchemaExpr.string("default")
        ))
        val input  = record("email" -> DynamicValue.Null)
        val result = migration(input)
        assertTrue(result == Right(record("email" -> str("default"))))
      },
      test("passes through non-option value") {
        val migration = DynamicMigration(MigrationAction.Mandate(
          DynamicOptic.root, "name", DynamicSchemaExpr.string("default")
        ))
        val input  = record("name" -> str("Alice"))
        val result = migration(input)
        assertTrue(result == Right(record("name" -> str("Alice"))))
      }
    ),
    suite("Optionalize")(
      test("wraps value in Some") {
        val migration = DynamicMigration(MigrationAction.Optionalize(DynamicOptic.root, "name"))
        val input  = record("name" -> str("Alice"))
        val result = migration(input)
        assertTrue(result == Right(record("name" -> DynamicValue.Variant("Some", str("Alice")))))
      }
    ),
    suite("ChangeType")(
      test("converts field type") {
        val migration = DynamicMigration(MigrationAction.ChangeType(
          DynamicOptic.root, "count",
          DynamicSchemaExpr.PrimitiveConversion(DynamicSchemaExpr.field("count"), ConversionType.IntToLong)
        ))
        val input  = record("count" -> int(42))
        val result = migration(input)
        assertTrue(result == Right(record("count" -> long(42L))))
      }
    ),
    suite("RenameCase")(
      test("renames matching variant case") {
        val migration = DynamicMigration(MigrationAction.RenameCase(
          DynamicOptic.root, "OldName", "NewName"
        ))
        val input  = DynamicValue.Variant("OldName", int(1))
        val result = migration(input)
        assertTrue(result == Right(DynamicValue.Variant("NewName", int(1))))
      },
      test("leaves non-matching case unchanged") {
        val migration = DynamicMigration(MigrationAction.RenameCase(
          DynamicOptic.root, "OldName", "NewName"
        ))
        val input  = DynamicValue.Variant("Other", int(1))
        val result = migration(input)
        assertTrue(result == Right(DynamicValue.Variant("Other", int(1))))
      },
      test("fails on non-variant") {
        val migration = DynamicMigration(MigrationAction.RenameCase(
          DynamicOptic.root, "A", "B"
        ))
        val result = migration(record("x" -> int(1)))
        assertTrue(result.isLeft)
      }
    ),
    suite("TransformCase")(
      test("applies nested migration to matching case") {
        val innerMigration = DynamicMigration(MigrationAction.AddField(
          DynamicOptic.root, "extra", DynamicSchemaExpr.int(0)
        ))
        val migration = DynamicMigration(MigrationAction.TransformCase(
          DynamicOptic.root, "MyCase", innerMigration
        ))
        val input  = DynamicValue.Variant("MyCase", DynamicValue.Record.empty)
        val result = migration(input)
        assertTrue(result == Right(DynamicValue.Variant("MyCase", record("extra" -> int(0)))))
      },
      test("leaves non-matching case unchanged") {
        val innerMigration = DynamicMigration(MigrationAction.AddField(
          DynamicOptic.root, "extra", DynamicSchemaExpr.int(0)
        ))
        val migration = DynamicMigration(MigrationAction.TransformCase(
          DynamicOptic.root, "MyCase", innerMigration
        ))
        val input  = DynamicValue.Variant("Other", record("x" -> int(1)))
        val result = migration(input)
        assertTrue(result == Right(input))
      }
    ),
    suite("TransformElements")(
      test("transforms all sequence elements") {
        val migration = DynamicMigration(MigrationAction.TransformElements(
          DynamicOptic.root,
          DynamicSchemaExpr.PrimitiveConversion(DynamicSchemaExpr.identity, ConversionType.IntToLong)
        ))
        val input  = DynamicValue.Sequence(zio.blocks.chunk.Chunk(int(1), int(2), int(3)))
        val result = migration(input)
        assertTrue(result == Right(DynamicValue.Sequence(zio.blocks.chunk.Chunk(long(1L), long(2L), long(3L)))))
      },
      test("handles empty sequence") {
        val migration = DynamicMigration(MigrationAction.TransformElements(
          DynamicOptic.root,
          DynamicSchemaExpr.PrimitiveConversion(DynamicSchemaExpr.identity, ConversionType.IntToLong)
        ))
        val result = migration(DynamicValue.Sequence(zio.blocks.chunk.Chunk.empty))
        assertTrue(result == Right(DynamicValue.Sequence(zio.blocks.chunk.Chunk.empty)))
      }
    ),
    suite("TransformKeys")(
      test("transforms all map keys") {
        val migration = DynamicMigration(MigrationAction.TransformKeys(
          DynamicOptic.root,
          DynamicSchemaExpr.PrimitiveConversion(DynamicSchemaExpr.identity, ConversionType.IntToString)
        ))
        val input = DynamicValue.Map(zio.blocks.chunk.Chunk(
          (int(1), str("a")),
          (int(2), str("b"))
        ))
        val result = migration(input)
        assertTrue(result == Right(DynamicValue.Map(zio.blocks.chunk.Chunk(
          (str("1"), str("a")),
          (str("2"), str("b"))
        ))))
      }
    ),
    suite("TransformValues")(
      test("transforms all map values") {
        val migration = DynamicMigration(MigrationAction.TransformValues(
          DynamicOptic.root,
          DynamicSchemaExpr.PrimitiveConversion(DynamicSchemaExpr.identity, ConversionType.IntToLong)
        ))
        val input = DynamicValue.Map(zio.blocks.chunk.Chunk(
          (str("a"), int(1)),
          (str("b"), int(2))
        ))
        val result = migration(input)
        assertTrue(result == Right(DynamicValue.Map(zio.blocks.chunk.Chunk(
          (str("a"), long(1L)),
          (str("b"), long(2L))
        ))))
      }
    ),
    suite("ApplyMigration")(
      test("applies nested migration at path") {
        val innerMigration = DynamicMigration(MigrationAction.Rename(
          DynamicOptic.root, "street", "streetName"
        ))
        val migration = DynamicMigration(MigrationAction.ApplyMigration(
          DynamicOptic.root.field("address"), innerMigration
        ))
        val input = record(
          "name"    -> str("Alice"),
          "address" -> record("street" -> str("123 Main St"))
        )
        val result = migration(input)
        assertTrue(result == Right(record(
          "name"    -> str("Alice"),
          "address" -> record("streetName" -> str("123 Main St"))
        )))
      }
    ),
    suite("Irreversible")(
      test("always fails when applied") {
        val original = MigrationAction.TransformValue(
          DynamicOptic.root, "x", DynamicSchemaExpr.int(1)
        )
        val migration = DynamicMigration(MigrationAction.Irreversible(DynamicOptic.root, original))
        val result = migration(record("x" -> int(1)))
        assertTrue(result.isLeft)
      }
    ),
    suite("nested path navigation")(
      test("adds field in nested record") {
        val migration = DynamicMigration(MigrationAction.AddField(
          DynamicOptic.root.field("address"), "zip", DynamicSchemaExpr.string("00000")
        ))
        val input = record(
          "name"    -> str("Alice"),
          "address" -> record("city" -> str("NYC"))
        )
        val result = migration(input)
        assertTrue(result == Right(record(
          "name"    -> str("Alice"),
          "address" -> record("city" -> str("NYC"), "zip" -> str("00000"))
        )))
      },
      test("renames field in deeply nested record") {
        val migration = DynamicMigration(MigrationAction.Rename(
          DynamicOptic.root.field("a").field("b"), "x", "y"
        ))
        val input = record(
          "a" -> record("b" -> record("x" -> int(1)))
        )
        val result = migration(input)
        assertTrue(result == Right(record(
          "a" -> record("b" -> record("y" -> int(1)))
        )))
      },
      test("fails when nested path not found") {
        val migration = DynamicMigration(MigrationAction.Rename(
          DynamicOptic.root.field("missing"), "x", "y"
        ))
        val result = migration(record("other" -> int(1)))
        assertTrue(result.isLeft)
      },
      test("modifies element in sequence via elements traversal") {
        val migration = DynamicMigration(MigrationAction.AddField(
          DynamicOptic.root.field("items").elements, "done", DynamicSchemaExpr.boolean(false)
        ))
        val input = record(
          "items" -> DynamicValue.Sequence(zio.blocks.chunk.Chunk(
            record("task" -> str("A")),
            record("task" -> str("B"))
          ))
        )
        val result = migration(input)
        assertTrue(result == Right(record(
          "items" -> DynamicValue.Sequence(zio.blocks.chunk.Chunk(
            record("task" -> str("A"), "done" -> DynamicValue.Primitive(new PrimitiveValue.Boolean(false))),
            record("task" -> str("B"), "done" -> DynamicValue.Primitive(new PrimitiveValue.Boolean(false)))
          ))
        )))
      },
      test("modifies values in map via mapValues traversal") {
        val migration = DynamicMigration(MigrationAction.AddField(
          DynamicOptic.root.field("data").mapValues, "version", DynamicSchemaExpr.int(2)
        ))
        val input = record(
          "data" -> DynamicValue.Map(zio.blocks.chunk.Chunk(
            (str("key1"), record("value" -> int(1)))
          ))
        )
        val result = migration(input)
        assertTrue(result == Right(record(
          "data" -> DynamicValue.Map(zio.blocks.chunk.Chunk(
            (str("key1"), record("value" -> int(1), "version" -> int(2)))
          ))
        )))
      }
    ),
    suite("composition")(
      test("applies multiple actions in order") {
        val migration = new DynamicMigration(Vector(
          MigrationAction.AddField(DynamicOptic.root, "age", DynamicSchemaExpr.int(0)),
          MigrationAction.Rename(DynamicOptic.root, "firstName", "name")
        ))
        val input  = record("firstName" -> str("Alice"))
        val result = migration(input)
        assertTrue(result == Right(record("name" -> str("Alice"), "age" -> int(0))))
      },
      test("++ composes migrations") {
        val m1 = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "x", DynamicSchemaExpr.int(1)))
        val m2 = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "y", DynamicSchemaExpr.int(2)))
        val combined = m1 ++ m2
        val result = combined(DynamicValue.Record.empty)
        assertTrue(result == Right(record("x" -> int(1), "y" -> int(2))))
      },
      test("identity is left identity") {
        val m = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "x", DynamicSchemaExpr.int(1)))
        val combined = DynamicMigration.identity ++ m
        val result = combined(DynamicValue.Record.empty)
        assertTrue(result == Right(record("x" -> int(1))))
      },
      test("identity is right identity") {
        val m = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "x", DynamicSchemaExpr.int(1)))
        val combined = m ++ DynamicMigration.identity
        val result = combined(DynamicValue.Record.empty)
        assertTrue(result == Right(record("x" -> int(1))))
      }
    ),
    suite("reverse")(
      test("reverse of AddField is DropField") {
        val add     = MigrationAction.AddField(DynamicOptic.root, "x", DynamicSchemaExpr.int(0))
        val reverse = add.reverse
        assertTrue(reverse.isInstanceOf[MigrationAction.DropField])
      },
      test("reverse of DropField is AddField") {
        val drop    = MigrationAction.DropField(DynamicOptic.root, "x", DynamicSchemaExpr.int(0))
        val reverse = drop.reverse
        assertTrue(reverse.isInstanceOf[MigrationAction.AddField])
      },
      test("reverse of Rename swaps from/to") {
        val rename  = MigrationAction.Rename(DynamicOptic.root, "a", "b")
        val reverse = rename.reverse.asInstanceOf[MigrationAction.Rename]
        assertTrue(reverse.from == "b" && reverse.to == "a")
      },
      test("reverse of TransformValue is Irreversible") {
        val transform = MigrationAction.TransformValue(DynamicOptic.root, "x", DynamicSchemaExpr.int(0))
        val reverse   = transform.reverse
        assertTrue(reverse.isInstanceOf[MigrationAction.Irreversible])
      },
      test("reverse of Mandate is Optionalize") {
        val mandate = MigrationAction.Mandate(DynamicOptic.root, "x", DynamicSchemaExpr.nil)
        val reverse = mandate.reverse
        assertTrue(reverse.isInstanceOf[MigrationAction.Optionalize])
      },
      test("reverse of Optionalize is Mandate") {
        val opt     = MigrationAction.Optionalize(DynamicOptic.root, "x")
        val reverse = opt.reverse
        assertTrue(reverse.isInstanceOf[MigrationAction.Mandate])
      },
      test("reverse of RenameCase swaps from/to") {
        val rc      = MigrationAction.RenameCase(DynamicOptic.root, "A", "B")
        val reverse = rc.reverse.asInstanceOf[MigrationAction.RenameCase]
        assertTrue(reverse.from == "B" && reverse.to == "A")
      },
      test("double reverse is structurally equal for Rename") {
        val rename = MigrationAction.Rename(DynamicOptic.root, "a", "b")
        val result = rename.reverse.reverse
        assertTrue(result == rename)
      },
      test("migration reverse reverses action order") {
        val m = new DynamicMigration(Vector(
          MigrationAction.Rename(DynamicOptic.root, "a", "b"),
          MigrationAction.AddField(DynamicOptic.root, "c", DynamicSchemaExpr.int(0))
        ))
        val reversed = m.reverse
        assertTrue(reversed.actions.length == 2)
        assertTrue(reversed.actions(0).isInstanceOf[MigrationAction.DropField])
        assertTrue(reversed.actions(1).isInstanceOf[MigrationAction.Rename])
      },
      test("add then reverse drop roundtrips") {
        val add = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "x", DynamicSchemaExpr.int(42)))
        val input = DynamicValue.Record.empty
        val forward = add(input)
        assertTrue(forward.isRight)
        val backward = add.reverse(forward.toOption.get)
        assertTrue(backward == Right(input))
      },
      test("rename then reverse roundtrips") {
        val m = DynamicMigration(MigrationAction.Rename(DynamicOptic.root, "old", "new"))
        val input  = record("old" -> int(1))
        val result = m(input).flatMap(m.reverse.apply)
        assertTrue(result == Right(input))
      }
    ),
    suite("optimize")(
      test("collapses consecutive renames") {
        val m = new DynamicMigration(Vector(
          MigrationAction.Rename(DynamicOptic.root, "a", "b"),
          MigrationAction.Rename(DynamicOptic.root, "b", "c")
        ))
        val optimized = m.optimize
        assertTrue(optimized.actions.length == 1)
        val rename = optimized.actions.head.asInstanceOf[MigrationAction.Rename]
        assertTrue(rename.from == "a" && rename.to == "c")
      },
      test("eliminates add-then-drop") {
        val m = new DynamicMigration(Vector(
          MigrationAction.AddField(DynamicOptic.root, "x", DynamicSchemaExpr.int(0)),
          MigrationAction.DropField(DynamicOptic.root, "x", DynamicSchemaExpr.int(0))
        ))
        val optimized = m.optimize
        assertTrue(optimized.isEmpty)
      },
      test("preserves non-optimizable actions") {
        val m = new DynamicMigration(Vector(
          MigrationAction.AddField(DynamicOptic.root, "x", DynamicSchemaExpr.int(0)),
          MigrationAction.AddField(DynamicOptic.root, "y", DynamicSchemaExpr.int(1))
        ))
        val optimized = m.optimize
        assertTrue(optimized.actions.length == 2)
      }
    ),
    suite("identity")(
      test("identity has no actions") {
        assertTrue(DynamicMigration.identity.isEmpty)
      },
      test("identity preserves any value") {
        val input  = record("x" -> int(1), "y" -> str("hello"))
        val result = DynamicMigration.identity(input)
        assertTrue(result == Right(input))
      }
    ),
    suite("toString")(
      test("empty migration") {
        assertTrue(DynamicMigration.identity.toString == "DynamicMigration {}")
      },
      test("non-empty migration shows actions") {
        val m = DynamicMigration(MigrationAction.Rename(DynamicOptic.root, "a", "b"))
        assertTrue(m.toString.contains("Rename"))
      }
    )
  )
}
