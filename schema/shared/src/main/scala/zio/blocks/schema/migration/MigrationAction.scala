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
 * A `MigrationAction` represents a single, atomic structural transformation in
 * a schema migration.
 *
 * All actions are pure data — no closures, no reflection, no side effects.
 * This makes them fully serializable and suitable for storage in migration
 * registries, transmission over the wire, and dynamic application.
 *
 * Each action operates at a specific path in the document tree, identified by
 * a [[DynamicOptic]]. Actions can be composed into a [[DynamicMigration]] and
 * structurally reversed via [[reverse]].
 *
 * ==Action Categories==
 *
 * '''Record operations:''' [[AddField]], [[DropField]], [[Rename]],
 * [[TransformValue]], [[Mandate]], [[Optionalize]], [[ChangeType]]
 *
 * '''Enum operations:''' [[RenameCase]], [[TransformCase]]
 *
 * '''Collection operations:''' [[TransformElements]], [[TransformKeys]],
 * [[TransformValues]]
 *
 * '''Composition:''' [[ApplyMigration]]
 *
 * '''Reversibility:''' [[Irreversible]]
 */
sealed trait MigrationAction {

  /**
   * The path in the document tree where this action operates.
   */
  def at: DynamicOptic

  /**
   * Returns the structural reverse of this action.
   *
   * For reversible actions (rename, add/drop pairs, mandate/optionalize), the
   * reverse undoes the transformation. For irreversible actions (value
   * transforms, type changes), the reverse is wrapped in [[Irreversible]].
   */
  def reverse: MigrationAction
}

object MigrationAction {

  // ── Record Operations ────────────────────────────────────────────────────

  /**
   * Adds a new field to a record at the given path.
   *
   * @param at
   *   path to the record containing the new field
   * @param fieldName
   *   name of the field to add
   * @param default
   *   expression providing the default value for the new field
   */
  final case class AddField(
    at: DynamicOptic,
    fieldName: String,
    default: DynamicSchemaExpr
  ) extends MigrationAction {
    def reverse: MigrationAction = DropField(at, fieldName, default)
  }

  /**
   * Removes a field from a record at the given path.
   *
   * @param at
   *   path to the record containing the field to remove
   * @param fieldName
   *   name of the field to remove
   * @param defaultForReverse
   *   expression that provides a default value when reversing this action
   *   (i.e., re-adding the field)
   */
  final case class DropField(
    at: DynamicOptic,
    fieldName: String,
    defaultForReverse: DynamicSchemaExpr
  ) extends MigrationAction {
    def reverse: MigrationAction = AddField(at, fieldName, defaultForReverse)
  }

  /**
   * Renames a field in a record.
   *
   * @param at
   *   path to the record containing the field
   * @param from
   *   original field name
   * @param to
   *   new field name
   */
  final case class Rename(
    at: DynamicOptic,
    from: String,
    to: String
  ) extends MigrationAction {
    def reverse: MigrationAction = Rename(at, to, from)
  }

  /**
   * Transforms a field value using a pure expression.
   *
   * @param at
   *   path to the record containing the field
   * @param fieldName
   *   name of the field to transform
   * @param transform
   *   expression to apply to the field value (receives the record as input)
   */
  final case class TransformValue(
    at: DynamicOptic,
    fieldName: String,
    transform: DynamicSchemaExpr
  ) extends MigrationAction {
    def reverse: MigrationAction = Irreversible(at, this)
  }

  /**
   * Converts an optional field to a required field.
   *
   * @param at
   *   path to the record containing the optional field
   * @param fieldName
   *   name of the field to mandate
   * @param default
   *   expression providing a default value when the field is None
   */
  final case class Mandate(
    at: DynamicOptic,
    fieldName: String,
    default: DynamicSchemaExpr
  ) extends MigrationAction {
    def reverse: MigrationAction = Optionalize(at, fieldName)
  }

  /**
   * Converts a required field to an optional field.
   *
   * @param at
   *   path to the record containing the field
   * @param fieldName
   *   name of the field to make optional
   */
  final case class Optionalize(
    at: DynamicOptic,
    fieldName: String
  ) extends MigrationAction {
    def reverse: MigrationAction = Mandate(at, fieldName, DynamicSchemaExpr.nil)
  }

  /**
   * Changes the type of a field using a converter expression.
   *
   * @param at
   *   path to the record containing the field
   * @param fieldName
   *   name of the field to change
   * @param converter
   *   expression that converts the old value to the new type
   */
  final case class ChangeType(
    at: DynamicOptic,
    fieldName: String,
    converter: DynamicSchemaExpr
  ) extends MigrationAction {
    def reverse: MigrationAction = Irreversible(at, this)
  }

  // ── Enum Operations ──────────────────────────────────────────────────────

  /**
   * Renames a case in a sum type (enum/sealed trait).
   *
   * @param at
   *   path to the variant
   * @param from
   *   original case name
   * @param to
   *   new case name
   */
  final case class RenameCase(
    at: DynamicOptic,
    from: String,
    to: String
  ) extends MigrationAction {
    def reverse: MigrationAction = RenameCase(at, to, from)
  }

  /**
   * Applies a nested migration to a specific case of a sum type.
   *
   * @param at
   *   path to the variant
   * @param caseName
   *   name of the case to transform
   * @param migration
   *   the migration to apply to the case's contents
   */
  final case class TransformCase(
    at: DynamicOptic,
    caseName: String,
    migration: DynamicMigration
  ) extends MigrationAction {
    def reverse: MigrationAction = TransformCase(at, caseName, migration.reverse)
  }

  // ── Collection Operations ────────────────────────────────────────────────

  /**
   * Transforms all elements in a sequence using an expression.
   *
   * @param at
   *   path to the sequence
   * @param transform
   *   expression to apply to each element
   */
  final case class TransformElements(
    at: DynamicOptic,
    transform: DynamicSchemaExpr
  ) extends MigrationAction {
    def reverse: MigrationAction = Irreversible(at, this)
  }

  /**
   * Transforms all keys in a map using an expression.
   *
   * @param at
   *   path to the map
   * @param transform
   *   expression to apply to each key
   */
  final case class TransformKeys(
    at: DynamicOptic,
    transform: DynamicSchemaExpr
  ) extends MigrationAction {
    def reverse: MigrationAction = Irreversible(at, this)
  }

  /**
   * Transforms all values in a map using an expression.
   *
   * @param at
   *   path to the map
   * @param transform
   *   expression to apply to each value
   */
  final case class TransformValues(
    at: DynamicOptic,
    transform: DynamicSchemaExpr
  ) extends MigrationAction {
    def reverse: MigrationAction = Irreversible(at, this)
  }

  // ── Composition ──────────────────────────────────────────────────────────

  /**
   * Applies a nested migration at a specific path.
   *
   * This enables recursive composition of migrations for nested schemas.
   *
   * @param at
   *   path where the nested migration applies
   * @param migration
   *   the nested migration to apply
   */
  final case class ApplyMigration(
    at: DynamicOptic,
    migration: DynamicMigration
  ) extends MigrationAction {
    def reverse: MigrationAction = ApplyMigration(at, migration.reverse)
  }

  // ── Reversibility ────────────────────────────────────────────────────────

  /**
   * Marks an action as irreversible. Attempting to apply this action will
   * always fail with an error indicating the original action cannot be undone.
   *
   * @param at
   *   path where the original action operated
   * @param original
   *   the original action that produced this irreversible marker
   */
  final case class Irreversible(
    at: DynamicOptic,
    original: MigrationAction
  ) extends MigrationAction {
    def reverse: MigrationAction = this
  }
}
