package zio.blocks.schema.migration

import zio.blocks.schema._
import zio.test._

object MigrationActionSpec extends SchemaBaseSpec {
  def spec: Spec[TestEnvironment, Any] = suite("MigrationActionSpec")(
    suite("reverse symmetry")(
      test("AddField.reverse.reverse == AddField") {
        val action = MigrationAction.AddField(DynamicOptic.root, "x", DynamicSchemaExpr.int(0))
        assertTrue(action.reverse.reverse == action)
      },
      test("DropField.reverse.reverse == DropField") {
        val action = MigrationAction.DropField(DynamicOptic.root, "x", DynamicSchemaExpr.int(0))
        assertTrue(action.reverse.reverse == action)
      },
      test("Rename.reverse.reverse == Rename") {
        val action = MigrationAction.Rename(DynamicOptic.root, "a", "b")
        assertTrue(action.reverse.reverse == action)
      },
      test("TransformValue.reverse is Irreversible") {
        val action = MigrationAction.TransformValue(DynamicOptic.root, "x", DynamicSchemaExpr.int(0))
        assertTrue(action.reverse.isInstanceOf[MigrationAction.Irreversible])
      },
      test("ChangeType.reverse is Irreversible") {
        val action = MigrationAction.ChangeType(DynamicOptic.root, "x", DynamicSchemaExpr.int(0))
        assertTrue(action.reverse.isInstanceOf[MigrationAction.Irreversible])
      },
      test("Mandate.reverse is Optionalize") {
        val action = MigrationAction.Mandate(DynamicOptic.root, "x", DynamicSchemaExpr.nil)
        val rev = action.reverse
        assertTrue(rev.isInstanceOf[MigrationAction.Optionalize])
        assertTrue(rev.asInstanceOf[MigrationAction.Optionalize].fieldName == "x")
      },
      test("Optionalize.reverse is Mandate") {
        val action = MigrationAction.Optionalize(DynamicOptic.root, "x")
        val rev = action.reverse
        assertTrue(rev.isInstanceOf[MigrationAction.Mandate])
        assertTrue(rev.asInstanceOf[MigrationAction.Mandate].fieldName == "x")
      },
      test("RenameCase.reverse swaps from/to") {
        val action = MigrationAction.RenameCase(DynamicOptic.root, "Old", "New")
        val rev = action.reverse.asInstanceOf[MigrationAction.RenameCase]
        assertTrue(rev.from == "New" && rev.to == "Old")
      },
      test("TransformCase.reverse reverses inner migration") {
        val inner = DynamicMigration(MigrationAction.Rename(DynamicOptic.root, "a", "b"))
        val action = MigrationAction.TransformCase(DynamicOptic.root, "Case1", inner)
        val rev = action.reverse.asInstanceOf[MigrationAction.TransformCase]
        assertTrue(rev.caseName == "Case1")
        val innerReverse = rev.migration.actions.head.asInstanceOf[MigrationAction.Rename]
        assertTrue(innerReverse.from == "b" && innerReverse.to == "a")
      },
      test("TransformElements.reverse is Irreversible") {
        val action = MigrationAction.TransformElements(DynamicOptic.root, DynamicSchemaExpr.identity)
        assertTrue(action.reverse.isInstanceOf[MigrationAction.Irreversible])
      },
      test("TransformKeys.reverse is Irreversible") {
        val action = MigrationAction.TransformKeys(DynamicOptic.root, DynamicSchemaExpr.identity)
        assertTrue(action.reverse.isInstanceOf[MigrationAction.Irreversible])
      },
      test("TransformValues.reverse is Irreversible") {
        val action = MigrationAction.TransformValues(DynamicOptic.root, DynamicSchemaExpr.identity)
        assertTrue(action.reverse.isInstanceOf[MigrationAction.Irreversible])
      },
      test("ApplyMigration.reverse reverses inner migration") {
        val inner = DynamicMigration(MigrationAction.Rename(DynamicOptic.root, "x", "y"))
        val action = MigrationAction.ApplyMigration(DynamicOptic.root, inner)
        val rev = action.reverse.asInstanceOf[MigrationAction.ApplyMigration]
        val innerReverse = rev.migration.actions.head.asInstanceOf[MigrationAction.Rename]
        assertTrue(innerReverse.from == "y" && innerReverse.to == "x")
      },
      test("Irreversible.reverse is itself") {
        val original = MigrationAction.TransformValue(DynamicOptic.root, "x", DynamicSchemaExpr.int(0))
        val irreversible = MigrationAction.Irreversible(DynamicOptic.root, original)
        assertTrue(irreversible.reverse eq irreversible)
      }
    ),
    suite("action paths")(
      test("all actions expose their path via at") {
        val root = DynamicOptic.root
        val nested = DynamicOptic.root.field("inner")

        val actions: Vector[MigrationAction] = Vector(
          MigrationAction.AddField(root, "x", DynamicSchemaExpr.int(0)),
          MigrationAction.DropField(root, "x", DynamicSchemaExpr.nil),
          MigrationAction.Rename(nested, "a", "b"),
          MigrationAction.TransformValue(root, "x", DynamicSchemaExpr.int(0)),
          MigrationAction.Mandate(root, "x", DynamicSchemaExpr.nil),
          MigrationAction.Optionalize(root, "x"),
          MigrationAction.ChangeType(root, "x", DynamicSchemaExpr.int(0)),
          MigrationAction.RenameCase(root, "A", "B"),
          MigrationAction.TransformCase(root, "A", DynamicMigration.identity),
          MigrationAction.TransformElements(root, DynamicSchemaExpr.identity),
          MigrationAction.TransformKeys(root, DynamicSchemaExpr.identity),
          MigrationAction.TransformValues(root, DynamicSchemaExpr.identity),
          MigrationAction.ApplyMigration(root, DynamicMigration.identity),
          MigrationAction.Irreversible(root, MigrationAction.Rename(root, "a", "b"))
        )

        assertTrue(actions.forall(a => a.at != null))
        assertTrue(actions(2).at == nested) // Rename at nested path
      }
    ),
    suite("AddField/DropField duality")(
      test("AddField reverse is DropField with same params") {
        val add = MigrationAction.AddField(DynamicOptic.root, "x", DynamicSchemaExpr.int(42))
        val drop = add.reverse.asInstanceOf[MigrationAction.DropField]
        assertTrue(drop.at == add.at)
        assertTrue(drop.fieldName == add.fieldName)
        assertTrue(drop.defaultForReverse == add.default)
      },
      test("DropField reverse is AddField with same params") {
        val drop = MigrationAction.DropField(DynamicOptic.root, "x", DynamicSchemaExpr.int(42))
        val add = drop.reverse.asInstanceOf[MigrationAction.AddField]
        assertTrue(add.at == drop.at)
        assertTrue(add.fieldName == drop.fieldName)
        assertTrue(add.default == drop.defaultForReverse)
      }
    )
  )
}
