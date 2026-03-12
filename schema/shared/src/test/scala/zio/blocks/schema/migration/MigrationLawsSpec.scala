package zio.blocks.schema.migration

import zio.blocks.schema._
import zio.test._

/**
 * Verifies the algebraic laws of the migration system:
 *
 *   1. Identity: `m ++ identity == m == identity ++ m` (by output)
 *   2. Associativity: `(m1 ++ m2) ++ m3 == m1 ++ (m2 ++ m3)` (by output)
 *   3. Reverse involution: `m.reverse.reverse == m` (structurally)
 *   4. Best-effort semantic inverse: for reversible migrations,
 *      `m.apply(a).flatMap(m.reverse.apply) == Right(a)`
 */
object MigrationLawsSpec extends SchemaBaseSpec {
  private def record(fields: (String, DynamicValue)*): DynamicValue = DynamicValue.Record(fields: _*)
  private def str(s: String): DynamicValue                         = DynamicValue.Primitive(new PrimitiveValue.String(s))
  private def int(i: Int): DynamicValue                            = DynamicValue.Primitive(new PrimitiveValue.Int(i))

  def spec: Spec[TestEnvironment, Any] = suite("MigrationLawsSpec")(
    suite("Identity law")(
      test("left identity: identity ++ m == m (by output)") {
        val m     = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "x", DynamicSchemaExpr.int(1)))
        val input = DynamicValue.Record.empty
        val left  = (DynamicMigration.identity ++ m)(input)
        val right = m(input)
        assertTrue(left == right)
      },
      test("right identity: m ++ identity == m (by output)") {
        val m     = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "x", DynamicSchemaExpr.int(1)))
        val input = DynamicValue.Record.empty
        val left  = (m ++ DynamicMigration.identity)(input)
        val right = m(input)
        assertTrue(left == right)
      },
      test("identity preserves any record") {
        val input = record("a" -> int(1), "b" -> str("hello"))
        assertTrue(DynamicMigration.identity(input) == Right(input))
      },
      test("identity preserves variant") {
        val input = DynamicValue.Variant("Foo", int(42))
        assertTrue(DynamicMigration.identity(input) == Right(input))
      },
      test("identity preserves sequence") {
        val input = DynamicValue.Sequence(zio.blocks.chunk.Chunk(int(1), int(2)))
        assertTrue(DynamicMigration.identity(input) == Right(input))
      },
      test("identity preserves null") {
        assertTrue(DynamicMigration.identity(DynamicValue.Null) == Right(DynamicValue.Null))
      }
    ),
    suite("Associativity law")(
      test("(m1 ++ m2) ++ m3 == m1 ++ (m2 ++ m3) for addField operations") {
        val m1 = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "a", DynamicSchemaExpr.int(1)))
        val m2 = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "b", DynamicSchemaExpr.int(2)))
        val m3 = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "c", DynamicSchemaExpr.int(3)))

        val input  = DynamicValue.Record.empty
        val left   = ((m1 ++ m2) ++ m3)(input)
        val right  = (m1 ++ (m2 ++ m3))(input)
        assertTrue(left == right)
      },
      test("associativity with mixed operations") {
        val m1 = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "x", DynamicSchemaExpr.int(0)))
        val m2 = DynamicMigration(MigrationAction.Rename(DynamicOptic.root, "x", "y"))
        val m3 = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "z", DynamicSchemaExpr.int(1)))

        val input  = DynamicValue.Record.empty
        val left   = ((m1 ++ m2) ++ m3)(input)
        val right  = (m1 ++ (m2 ++ m3))(input)
        assertTrue(left == right)
      },
      test("action vector concatenation is associative") {
        val m1 = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "a", DynamicSchemaExpr.int(1)))
        val m2 = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "b", DynamicSchemaExpr.int(2)))
        val m3 = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "c", DynamicSchemaExpr.int(3)))

        val left  = (m1 ++ m2) ++ m3
        val right = m1 ++ (m2 ++ m3)
        assertTrue(left.actions == right.actions)
      }
    ),
    suite("Reverse involution law")(
      test("m.reverse.reverse == m for Rename") {
        val m = DynamicMigration(MigrationAction.Rename(DynamicOptic.root, "a", "b"))
        assertTrue(m.reverse.reverse.actions == m.actions)
      },
      test("m.reverse.reverse == m for AddField") {
        val m = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "x", DynamicSchemaExpr.int(0)))
        assertTrue(m.reverse.reverse.actions == m.actions)
      },
      test("m.reverse.reverse == m for DropField") {
        val m = DynamicMigration(MigrationAction.DropField(DynamicOptic.root, "x", DynamicSchemaExpr.int(0)))
        assertTrue(m.reverse.reverse.actions == m.actions)
      },
      test("m.reverse.reverse == m for RenameCase") {
        val m = DynamicMigration(MigrationAction.RenameCase(DynamicOptic.root, "A", "B"))
        assertTrue(m.reverse.reverse.actions == m.actions)
      },
      test("m.reverse.reverse preserves action count for multi-action migration") {
        val m = new DynamicMigration(Vector(
          MigrationAction.Rename(DynamicOptic.root, "a", "b"),
          MigrationAction.AddField(DynamicOptic.root, "c", DynamicSchemaExpr.int(0)),
          MigrationAction.RenameCase(DynamicOptic.root, "X", "Y")
        ))
        val result = m.reverse.reverse
        assertTrue(result.actions.length == m.actions.length)
      },
      test("double reverse of Mandate->Optionalize chain") {
        val m = new DynamicMigration(Vector(
          MigrationAction.Mandate(DynamicOptic.root, "x", DynamicSchemaExpr.nil),
          MigrationAction.Optionalize(DynamicOptic.root, "y")
        ))
        val rr = m.reverse.reverse
        assertTrue(rr.actions(0).isInstanceOf[MigrationAction.Mandate])
        assertTrue(rr.actions(1).isInstanceOf[MigrationAction.Optionalize])
      }
    ),
    suite("Semantic inverse law")(
      test("rename roundtrips") {
        val m     = DynamicMigration(MigrationAction.Rename(DynamicOptic.root, "old", "new"))
        val input = record("old" -> int(42))
        val result = m(input).flatMap(m.reverse.apply)
        assertTrue(result == Right(input))
      },
      test("add/drop roundtrips") {
        val m     = DynamicMigration(MigrationAction.AddField(DynamicOptic.root, "x", DynamicSchemaExpr.int(99)))
        val input = DynamicValue.Record.empty
        val result = m(input).flatMap(m.reverse.apply)
        assertTrue(result == Right(input))
      },
      test("rename case roundtrips") {
        val m     = DynamicMigration(MigrationAction.RenameCase(DynamicOptic.root, "Foo", "Bar"))
        val input = DynamicValue.Variant("Foo", int(1))
        val result = m(input).flatMap(m.reverse.apply)
        assertTrue(result == Right(input))
      },
      test("optionalize then mandate roundtrips") {
        val m = new DynamicMigration(Vector(
          MigrationAction.Optionalize(DynamicOptic.root, "x")
        ))
        val input = record("x" -> int(42))
        val forward = m(input)
        assertTrue(forward.isRight)
        // Reverse of Optionalize is Mandate with nil default
        // Since the value is Some(42), mandate should extract it
        val backward = m.reverse(forward.toOption.get)
        assertTrue(backward == Right(input))
      },
      test("multi-step reversible migration roundtrips") {
        val m = new DynamicMigration(Vector(
          MigrationAction.Rename(DynamicOptic.root, "firstName", "name"),
          MigrationAction.AddField(DynamicOptic.root, "version", DynamicSchemaExpr.int(2))
        ))
        val input  = record("firstName" -> str("Alice"))
        val result = m(input).flatMap(m.reverse.apply)
        assertTrue(result == Right(input))
      },
      test("nested migration roundtrips") {
        val inner = DynamicMigration(MigrationAction.Rename(DynamicOptic.root, "street", "streetName"))
        val m     = DynamicMigration(MigrationAction.ApplyMigration(DynamicOptic.root.field("address"), inner))
        val input = record(
          "name"    -> str("Alice"),
          "address" -> record("street" -> str("123 Main"))
        )
        val result = m(input).flatMap(m.reverse.apply)
        assertTrue(result == Right(input))
      }
    )
  )
}
