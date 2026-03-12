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
 * A type-safe migration that transforms values of type `A` to type `B`.
 *
 * `Migration[A, B]` wraps a [[DynamicMigration]] with source and target
 * schemas, providing type-safe application. The underlying `DynamicMigration`
 * is fully serializable and contains no closures.
 *
 * ==Usage==
 *
 * {{{
 * case class PersonV1(firstName: String, lastName: String)
 * case class PersonV2(fullName: String, age: Int)
 *
 * val migration = Migration.builder[PersonV1, PersonV2]
 *   .addField("age", DynamicSchemaExpr.int(0))
 *   .dropField("firstName", DynamicSchemaExpr.string(""))
 *   .dropField("lastName", DynamicSchemaExpr.string(""))
 *   .addField("fullName", DynamicSchemaExpr.string("Unknown"))
 *   .build
 *
 * val result: Either[SchemaError, PersonV2] = migration(personV1)
 * }}}
 *
 * ==Algebraic Laws==
 *
 * Migrations satisfy the following laws:
 *
 *   - '''Identity:''' `Migration.identity[A].apply(a) == Right(a)`
 *   - '''Composition associativity:'''
 *     `(m1 ++ m2).dynamicMigration == m1.dynamicMigration ++ m2.dynamicMigration`
 *   - '''Reverse involution:''' `m.reverse.reverse.dynamicMigration ==
 *     m.dynamicMigration`
 *
 * @param dynamicMigration
 *   the underlying untyped migration
 * @param sourceSchema
 *   schema for the source type A
 * @param targetSchema
 *   schema for the target type B
 */
final case class Migration[A, B](
  dynamicMigration: DynamicMigration,
  sourceSchema: Schema[A],
  targetSchema: Schema[B]
) {

  /**
   * Applies this migration to a value of type A, producing either a
   * [[SchemaError]] or a value of type B.
   */
  def apply(value: A): Either[SchemaError, B] = {
    val dynamic = sourceSchema.toDynamicValue(value)
    dynamicMigration(dynamic).flatMap(targetSchema.fromDynamicValue)
  }

  /**
   * Composes this migration with another, producing a migration from A to C.
   */
  def ++[C](that: Migration[B, C]): Migration[A, C] =
    Migration(
      dynamicMigration ++ that.dynamicMigration,
      sourceSchema,
      that.targetSchema
    )

  /**
   * Alias for [[++]].
   */
  def andThen[C](that: Migration[B, C]): Migration[A, C] = this ++ that

  /**
   * Returns the structural reverse of this migration.
   */
  def reverse: Migration[B, A] =
    Migration(dynamicMigration.reverse, targetSchema, sourceSchema)

  /** Returns `true` if this migration has no actions. */
  def isEmpty: Boolean = dynamicMigration.isEmpty

  /** Returns the number of actions in this migration. */
  def size: Int = dynamicMigration.size

  /**
   * Returns an optimized version of this migration.
   */
  def optimize: Migration[A, B] =
    Migration(dynamicMigration.optimize, sourceSchema, targetSchema)
}

object Migration {

  /**
   * Creates an identity migration that performs no transformations.
   */
  def identity[A](implicit schema: Schema[A]): Migration[A, A] =
    Migration(DynamicMigration.identity, schema, schema)

  /**
   * Creates a new [[MigrationBuilder]] for constructing migrations from A to B.
   */
  def builder[A, B](implicit sourceSchema: Schema[A], targetSchema: Schema[B]): MigrationBuilder[A, B] =
    new MigrationBuilder[A, B](sourceSchema, targetSchema, Vector.empty)
}

/**
 * A fluent builder for constructing [[Migration]] instances.
 *
 * The builder accumulates [[MigrationAction]] values and produces a
 * [[Migration]] via [[build]] (with validation) or [[buildPartial]] (without
 * validation).
 *
 * All builder methods return a new builder instance, enabling safe composition.
 *
 * @param sourceSchema
 *   schema for the source type A
 * @param targetSchema
 *   schema for the target type B
 * @param actions
 *   accumulated migration actions
 */
final class MigrationBuilder[A, B](
  val sourceSchema: Schema[A],
  val targetSchema: Schema[B],
  val actions: Vector[MigrationAction]
) {

  // ── Record Operations ────────────────────────────────────────────────

  /**
   * Adds a field to the target record with a default value.
   *
   * @param fieldName
   *   name of the field in the target schema
   * @param default
   *   expression producing the default value
   */
  def addField(fieldName: String, default: DynamicSchemaExpr): MigrationBuilder[A, B] =
    addAction(MigrationAction.AddField(DynamicOptic.root, fieldName, default))

  /**
   * Adds a field at a nested path.
   */
  def addFieldAt(path: DynamicOptic, fieldName: String, default: DynamicSchemaExpr): MigrationBuilder[A, B] =
    addAction(MigrationAction.AddField(path, fieldName, default))

  /**
   * Drops a field from the source record.
   *
   * @param fieldName
   *   name of the field to remove
   * @param defaultForReverse
   *   default value to use when reversing this migration
   */
  def dropField(fieldName: String, defaultForReverse: DynamicSchemaExpr = DynamicSchemaExpr.nil): MigrationBuilder[A, B] =
    addAction(MigrationAction.DropField(DynamicOptic.root, fieldName, defaultForReverse))

  /**
   * Drops a field at a nested path.
   */
  def dropFieldAt(
    path: DynamicOptic,
    fieldName: String,
    defaultForReverse: DynamicSchemaExpr = DynamicSchemaExpr.nil
  ): MigrationBuilder[A, B] =
    addAction(MigrationAction.DropField(path, fieldName, defaultForReverse))

  /**
   * Renames a field.
   *
   * @param from
   *   original field name
   * @param to
   *   new field name
   */
  def renameField(from: String, to: String): MigrationBuilder[A, B] =
    addAction(MigrationAction.Rename(DynamicOptic.root, from, to))

  /**
   * Renames a field at a nested path.
   */
  def renameFieldAt(path: DynamicOptic, from: String, to: String): MigrationBuilder[A, B] =
    addAction(MigrationAction.Rename(path, from, to))

  /**
   * Transforms a field value using a pure expression.
   *
   * @param fieldName
   *   name of the field to transform
   * @param transform
   *   expression to apply (receives the record as input)
   */
  def transformField(fieldName: String, transform: DynamicSchemaExpr): MigrationBuilder[A, B] =
    addAction(MigrationAction.TransformValue(DynamicOptic.root, fieldName, transform))

  /**
   * Transforms a field at a nested path.
   */
  def transformFieldAt(
    path: DynamicOptic,
    fieldName: String,
    transform: DynamicSchemaExpr
  ): MigrationBuilder[A, B] =
    addAction(MigrationAction.TransformValue(path, fieldName, transform))

  /**
   * Makes an optional field required by unwrapping Option.
   *
   * @param fieldName
   *   name of the optional field
   * @param default
   *   expression providing a default when the field is None
   */
  def mandateField(fieldName: String, default: DynamicSchemaExpr): MigrationBuilder[A, B] =
    addAction(MigrationAction.Mandate(DynamicOptic.root, fieldName, default))

  /**
   * Makes a required field optional by wrapping in Option.
   *
   * @param fieldName
   *   name of the field to make optional
   */
  def optionalizeField(fieldName: String): MigrationBuilder[A, B] =
    addAction(MigrationAction.Optionalize(DynamicOptic.root, fieldName))

  /**
   * Changes the type of a field using a converter expression.
   *
   * @param fieldName
   *   name of the field to change
   * @param converter
   *   expression that converts the old value to the new type
   */
  def changeFieldType(fieldName: String, converter: DynamicSchemaExpr): MigrationBuilder[A, B] =
    addAction(MigrationAction.ChangeType(DynamicOptic.root, fieldName, converter))

  // ── Enum Operations ──────────────────────────────────────────────────

  /**
   * Renames a case in a sum type.
   *
   * @param from
   *   original case name
   * @param to
   *   new case name
   */
  def renameCase(from: String, to: String): MigrationBuilder[A, B] =
    addAction(MigrationAction.RenameCase(DynamicOptic.root, from, to))

  /**
   * Renames a case at a nested path.
   */
  def renameCaseAt(path: DynamicOptic, from: String, to: String): MigrationBuilder[A, B] =
    addAction(MigrationAction.RenameCase(path, from, to))

  /**
   * Applies a nested migration to a specific case of a sum type.
   */
  def transformCase(caseName: String, migration: DynamicMigration): MigrationBuilder[A, B] =
    addAction(MigrationAction.TransformCase(DynamicOptic.root, caseName, migration))

  // ── Collection Operations ────────────────────────────────────────────

  /**
   * Transforms all elements of a sequence at the given path.
   */
  def transformElements(path: DynamicOptic, transform: DynamicSchemaExpr): MigrationBuilder[A, B] =
    addAction(MigrationAction.TransformElements(path, transform))

  /**
   * Transforms all keys of a map at the given path.
   */
  def transformKeys(path: DynamicOptic, transform: DynamicSchemaExpr): MigrationBuilder[A, B] =
    addAction(MigrationAction.TransformKeys(path, transform))

  /**
   * Transforms all values of a map at the given path.
   */
  def transformValues(path: DynamicOptic, transform: DynamicSchemaExpr): MigrationBuilder[A, B] =
    addAction(MigrationAction.TransformValues(path, transform))

  // ── Nested Migration ─────────────────────────────────────────────────

  /**
   * Applies a nested migration at a specific path.
   */
  def applyMigration(path: DynamicOptic, migration: DynamicMigration): MigrationBuilder[A, B] =
    addAction(MigrationAction.ApplyMigration(path, migration))

  // ── Build ────────────────────────────────────────────────────────────

  /**
   * Builds the migration with validation.
   *
   * Validation checks that:
   *   - All target fields are either present in source, added, or renamed to
   *   - All source fields are either present in target, dropped, or renamed
   *     from
   *   - No conflicting operations exist
   *
   * @return
   *   either a validation error or the constructed migration
   */
  def build: Either[SchemaError, Migration[A, B]] = {
    validate.map(_ => buildPartial)
  }

  /**
   * Builds the migration without validation. Useful for partial migrations or
   * when the caller has already validated correctness.
   */
  def buildPartial: Migration[A, B] =
    Migration(new DynamicMigration(actions), sourceSchema, targetSchema)

  // ── Validation ───────────────────────────────────────────────────────

  private def validate: Either[SchemaError, Unit] = {
    val sourceFields = extractFieldNames(sourceSchema.reflect)
    val targetFields = extractFieldNames(targetSchema.reflect)

    if (sourceFields.isEmpty && targetFields.isEmpty) return Right(())

    // Track what happens to each field
    var addedFields   = Set.empty[String]
    var droppedFields = Set.empty[String]
    var renamedFrom   = Map.empty[String, String] // from -> to
    var renamedTo     = Map.empty[String, String] // to -> from

    actions.foreach {
      case MigrationAction.AddField(at, name, _) if at.nodes.isEmpty =>
        addedFields += name
      case MigrationAction.DropField(at, name, _) if at.nodes.isEmpty =>
        droppedFields += name
      case MigrationAction.Rename(at, from, to) if at.nodes.isEmpty =>
        renamedFrom += (from -> to)
        renamedTo += (to -> from)
      case _ => // Nested or non-field actions don't affect root validation
    }

    val errors = Vector.newBuilder[String]

    // Check all added fields exist in target
    addedFields.foreach { name =>
      if (!targetFields.contains(name))
        errors += s"AddField '$name' not found in target schema"
    }

    // Check all dropped fields exist in source
    droppedFields.foreach { name =>
      if (!sourceFields.contains(name))
        errors += s"DropField '$name' not found in source schema"
    }

    // Check renames
    renamedFrom.foreach { case (from, to) =>
      if (!sourceFields.contains(from))
        errors += s"Rename source '$from' not found in source schema"
      if (!targetFields.contains(to))
        errors += s"Rename target '$to' not found in target schema"
    }

    // Check unhandled target fields (fields in target not accounted for)
    targetFields.foreach { name =>
      val isInSource  = sourceFields.contains(name)
      val isAdded     = addedFields.contains(name)
      val isRenamedTo = renamedTo.contains(name)
      if (!isInSource && !isAdded && !isRenamedTo)
        errors += s"Target field '$name' is not covered by any migration action"
    }

    // Check unhandled source fields (fields in source that should be accounted for)
    sourceFields.foreach { name =>
      val isInTarget    = targetFields.contains(name)
      val isDropped     = droppedFields.contains(name)
      val isRenamedFrom = renamedFrom.contains(name)
      if (!isInTarget && !isDropped && !isRenamedFrom)
        errors += s"Source field '$name' is not handled by any migration action"
    }

    val result = errors.result()
    if (result.isEmpty) Right(())
    else Left(SchemaError(result.mkString("; ")))
  }

  private def extractFieldNames(reflect: Reflect.Bound[_]): Set[String] =
    reflect match {
      case r: Reflect.Record[_, _] =>
        val fields = r.fields
        var names  = Set.empty[String]
        var idx    = 0
        while (idx < fields.length) {
          names += fields(idx).name
          idx += 1
        }
        names
      case _ => Set.empty
    }

  private def addAction(action: MigrationAction): MigrationBuilder[A, B] =
    new MigrationBuilder(sourceSchema, targetSchema, actions :+ action)
}
