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

import zio.blocks.chunk.{Chunk, ChunkBuilder}
import zio.blocks.schema._

/**
 * An untyped, fully serializable migration that transforms [[DynamicValue]]
 * instances from one schema version to another.
 *
 * `DynamicMigration` is the core of the migration system. It contains no
 * closures, functions, or runtime-generated code — only pure data in the form
 * of [[MigrationAction]] values. This enables:
 *
 *   - '''Serialization:''' migrations can be stored in registries, databases,
 *     or transmitted over the wire
 *   - '''Introspection:''' migrations can be inspected, analyzed, and optimized
 *   - '''Offline application:''' migrations can generate SQL DDL/DML, Avro
 *     schema evolution rules, or other target-format transformations
 *
 * ==Algebraic Laws==
 *
 *   - '''Identity:''' `m ++ DynamicMigration.identity == m` and
 *     `DynamicMigration.identity ++ m == m`
 *   - '''Associativity:''' `(m1 ++ m2) ++ m3 == m1 ++ (m2 ++ m3)` (when
 *     applied to any value)
 *   - '''Reverse involution:''' `m.reverse.reverse` is structurally equal to
 *     `m`
 *   - '''Best-effort semantic inverse:''' For reversible migrations,
 *     `m.apply(a).flatMap(m.reverse.apply)` should yield `Right(a)`
 *
 * @param actions
 *   the ordered sequence of migration actions to apply
 */
final case class DynamicMigration(actions: Vector[MigrationAction]) {

  /**
   * Applies this migration to a [[DynamicValue]], returning either a
   * [[SchemaError]] or the transformed value.
   */
  def apply(value: DynamicValue): Either[SchemaError, DynamicValue] = {
    var current: DynamicValue                    = value
    val len                                      = actions.length
    var idx                                      = 0
    var error: Either[SchemaError, DynamicValue]  = null
    while (idx < len && error == null) {
      DynamicMigration.applyAction(current, actions(idx)) match {
        case Right(updated) => current = updated
        case left           => error = left
      }
      idx += 1
    }
    if (error != null) error else Right(current)
  }

  /**
   * Composes this migration with another. The resulting migration applies
   * `this` first, then `that`.
   */
  def ++(that: DynamicMigration): DynamicMigration =
    new DynamicMigration(this.actions ++ that.actions)

  /**
   * Alias for [[++]].
   */
  def andThen(that: DynamicMigration): DynamicMigration = this ++ that

  /**
   * Returns the structural reverse of this migration.
   *
   * The reverse migration applies the reversed actions in reverse order. For
   * reversible actions (rename, add/drop pairs), this produces a true inverse.
   * For irreversible actions (transforms, type changes), applying the reverse
   * will fail with an error.
   */
  def reverse: DynamicMigration =
    new DynamicMigration(actions.reverseIterator.map(_.reverse).toVector)

  /** Returns `true` if this migration has no actions. */
  def isEmpty: Boolean = actions.isEmpty

  /** Returns the number of actions in this migration. */
  def size: Int = actions.size

  /**
   * Returns an optimized version of this migration with redundant actions
   * eliminated.
   *
   * Current optimizations:
   *   - Consecutive renames of the same field are collapsed
   *   - Add immediately followed by drop of the same field are eliminated
   *   - Drop immediately followed by add of the same field are eliminated
   */
  def optimize: DynamicMigration =
    if (actions.length <= 1) this
    else {
      val optimized = Vector.newBuilder[MigrationAction]
      val len       = actions.length
      var idx       = 0
      while (idx < len) {
        val action = actions(idx)
        if (idx + 1 < len) {
          val next = actions(idx + 1)
          (action, next) match {
            // Collapse consecutive renames: rename(a→b) then rename(b→c) => rename(a→c)
            case (MigrationAction.Rename(at1, from, mid), MigrationAction.Rename(at2, mid2, to))
                if at1 == at2 && mid == mid2 =>
              optimized += MigrationAction.Rename(at1, from, to)
              idx += 2

            // Eliminate add-then-drop of same field
            case (MigrationAction.AddField(at1, name1, _), MigrationAction.DropField(at2, name2, _))
                if at1 == at2 && name1 == name2 =>
              idx += 2

            // Eliminate drop-then-add of same field (effectively a no-op if defaults match)
            case (MigrationAction.DropField(at1, name1, _), MigrationAction.AddField(at2, name2, _))
                if at1 == at2 && name1 == name2 =>
              idx += 2

            case _ =>
              optimized += action
              idx += 1
          }
        } else {
          optimized += action
          idx += 1
        }
      }
      new DynamicMigration(optimized.result())
    }

  override def toString: String =
    if (actions.isEmpty) "DynamicMigration {}"
    else {
      val sb = new java.lang.StringBuilder("DynamicMigration {\n")
      actions.foreach { a =>
        sb.append("  ").append(a.toString).append('\n')
      }
      sb.append('}').toString
    }
}

object DynamicMigration {

  /** The identity migration — applies no changes. */
  val identity: DynamicMigration = new DynamicMigration(Vector.empty)

  /** Creates a migration from a single action. */
  def apply(action: MigrationAction): DynamicMigration =
    new DynamicMigration(Vector(action))

  /** Creates a migration from multiple actions. */
  def apply(action: MigrationAction, actions: MigrationAction*): DynamicMigration =
    new DynamicMigration(action +: actions.toVector)

  // ── Action Execution ──────────────────────────────────────────────────

  private[migration] def applyAction(
    value: DynamicValue,
    action: MigrationAction
  ): Either[SchemaError, DynamicValue] =
    action match {
      case a: MigrationAction.AddField          => applyAddField(value, a)
      case a: MigrationAction.DropField         => applyDropField(value, a)
      case a: MigrationAction.Rename            => applyRename(value, a)
      case a: MigrationAction.TransformValue    => applyTransformValue(value, a)
      case a: MigrationAction.Mandate           => applyMandate(value, a)
      case a: MigrationAction.Optionalize       => applyOptionalize(value, a)
      case a: MigrationAction.ChangeType        => applyChangeType(value, a)
      case a: MigrationAction.RenameCase        => applyRenameCase(value, a)
      case a: MigrationAction.TransformCase     => applyTransformCase(value, a)
      case a: MigrationAction.TransformElements => applyTransformElements(value, a)
      case a: MigrationAction.TransformKeys     => applyTransformKeys(value, a)
      case a: MigrationAction.TransformValues   => applyTransformValues(value, a)
      case a: MigrationAction.ApplyMigration    => applyNestedMigration(value, a)
      case a: MigrationAction.Irreversible      =>
        Left(SchemaError.message(
          s"Cannot apply irreversible action (reverse of ${a.original.getClass.getSimpleName})",
          a.at
        ))
    }

  // ── Record Operations ─────────────────────────────────────────────────

  private def applyAddField(
    root: DynamicValue,
    action: MigrationAction.AddField
  ): Either[SchemaError, DynamicValue] =
    navigateAndModify(root, action.at) {
      case record: DynamicValue.Record =>
        // Check field doesn't already exist
        val fields = record.fields
        var idx    = 0
        var exists = false
        while (idx < fields.length && !exists) {
          if (fields(idx)._1 == action.fieldName) exists = true
          idx += 1
        }
        if (exists) Left(SchemaError.message(s"Field '${action.fieldName}' already exists", action.at))
        else action.default.eval(root).map { defaultValue =>
          DynamicValue.Record(fields.appended((action.fieldName, defaultValue)))
        }
      case _ =>
        Left(SchemaError.message(s"AddField: expected Record at path", action.at))
    }

  private def applyDropField(
    root: DynamicValue,
    action: MigrationAction.DropField
  ): Either[SchemaError, DynamicValue] =
    navigateAndModify(root, action.at) {
      case record: DynamicValue.Record =>
        val fields  = record.fields
        val builder = Vector.newBuilder[(String, DynamicValue)]
        var found   = false
        var idx     = 0
        while (idx < fields.length) {
          val (name, v) = fields(idx)
          if (name == action.fieldName) found = true
          else builder += ((name, v))
          idx += 1
        }
        if (!found) Left(SchemaError.message(s"DropField: field '${action.fieldName}' not found", action.at))
        else Right(new DynamicValue.Record(Chunk.from(builder.result())))
      case _ =>
        Left(SchemaError.message(s"DropField: expected Record at path", action.at))
    }

  private def applyRename(
    root: DynamicValue,
    action: MigrationAction.Rename
  ): Either[SchemaError, DynamicValue] =
    navigateAndModify(root, action.at) {
      case record: DynamicValue.Record =>
        val fields = record.fields
        // Check target doesn't exist and source does exist
        var sourceFound = false
        var targetFound = false
        var idx         = 0
        while (idx < fields.length) {
          val name = fields(idx)._1
          if (name == action.from) sourceFound = true
          if (name == action.to) targetFound = true
          idx += 1
        }
        if (!sourceFound)
          Left(SchemaError.message(s"Rename: source field '${action.from}' not found", action.at))
        else if (targetFound)
          Left(SchemaError.message(s"Rename: target field '${action.to}' already exists", action.at))
        else {
          val newFields = fields.map { case (name, v) =>
            if (name == action.from) (action.to, v) else (name, v)
          }
          Right(new DynamicValue.Record(newFields))
        }
      case _ =>
        Left(SchemaError.message(s"Rename: expected Record at path", action.at))
    }

  private def applyTransformValue(
    root: DynamicValue,
    action: MigrationAction.TransformValue
  ): Either[SchemaError, DynamicValue] =
    navigateAndModify(root, action.at) {
      case record: DynamicValue.Record =>
        val fields = record.fields
        var found  = false
        var idx    = 0
        while (idx < fields.length) {
          if (fields(idx)._1 == action.fieldName) { found = true; idx = fields.length }
          idx += 1
        }
        if (!found)
          Left(SchemaError.message(s"TransformValue: field '${action.fieldName}' not found", action.at))
        else
          action.transform.eval(root).map { newValue =>
            val newFields = fields.map { case (name, v) =>
              if (name == action.fieldName) (name, newValue) else (name, v)
            }
            new DynamicValue.Record(newFields)
          }
      case _ =>
        Left(SchemaError.message(s"TransformValue: expected Record at path", action.at))
    }

  private def applyMandate(
    root: DynamicValue,
    action: MigrationAction.Mandate
  ): Either[SchemaError, DynamicValue] =
    navigateAndModify(root, action.at) {
      case record: DynamicValue.Record =>
        val fields = record.fields
        var found  = false
        var resultFields = fields
        var idx    = 0
        var error: Either[SchemaError, DynamicValue] = null
        while (idx < fields.length && error == null) {
          val (name, v) = fields(idx)
          if (name == action.fieldName) {
            found = true
            val unwrapped = v match {
              case variant: DynamicValue.Variant if variant.caseNameValue == "Some" => Right(variant.value)
              case variant: DynamicValue.Variant if variant.caseNameValue == "None" => action.default.eval(root)
              case _: DynamicValue.Null.type                                       => action.default.eval(root)
              case other                                                           => Right(other)
            }
            unwrapped match {
              case Right(uv) =>
                resultFields = fields.updated(idx, (name, uv))
              case Left(e) => error = Left(e)
            }
          }
          idx += 1
        }
        if (error != null) error
        else if (!found)
          Left(SchemaError.message(s"Mandate: field '${action.fieldName}' not found", action.at))
        else
          Right(new DynamicValue.Record(resultFields))
      case _ =>
        Left(SchemaError.message(s"Mandate: expected Record at path", action.at))
    }

  private def applyOptionalize(
    root: DynamicValue,
    action: MigrationAction.Optionalize
  ): Either[SchemaError, DynamicValue] =
    navigateAndModify(root, action.at) {
      case record: DynamicValue.Record =>
        val fields = record.fields
        var found  = false
        var idx    = 0
        while (idx < fields.length) {
          if (fields(idx)._1 == action.fieldName) { found = true; idx = fields.length }
          idx += 1
        }
        if (!found)
          Left(SchemaError.message(s"Optionalize: field '${action.fieldName}' not found", action.at))
        else {
          val newFields = fields.map { case (name, v) =>
            if (name == action.fieldName) (name, DynamicValue.Variant("Some", v))
            else (name, v)
          }
          Right(new DynamicValue.Record(newFields))
        }
      case _ =>
        Left(SchemaError.message(s"Optionalize: expected Record at path", action.at))
    }

  private def applyChangeType(
    root: DynamicValue,
    action: MigrationAction.ChangeType
  ): Either[SchemaError, DynamicValue] =
    navigateAndModify(root, action.at) {
      case record: DynamicValue.Record =>
        val fields = record.fields
        var found  = false
        var idx    = 0
        while (idx < fields.length) {
          if (fields(idx)._1 == action.fieldName) { found = true; idx = fields.length }
          idx += 1
        }
        if (!found)
          Left(SchemaError.message(s"ChangeType: field '${action.fieldName}' not found", action.at))
        else
          action.converter.eval(root).map { newValue =>
            val newFields = fields.map { case (name, v) =>
              if (name == action.fieldName) (name, newValue) else (name, v)
            }
            new DynamicValue.Record(newFields)
          }
      case _ =>
        Left(SchemaError.message(s"ChangeType: expected Record at path", action.at))
    }

  // ── Enum Operations ───────────────────────────────────────────────────

  private def applyRenameCase(
    root: DynamicValue,
    action: MigrationAction.RenameCase
  ): Either[SchemaError, DynamicValue] =
    navigateAndModify(root, action.at) {
      case variant: DynamicValue.Variant =>
        if (variant.caseNameValue == action.from)
          Right(DynamicValue.Variant(action.to, variant.value))
        else
          Right(variant) // Not this case, leave unchanged
      case _ =>
        Left(SchemaError.message(s"RenameCase: expected Variant at path", action.at))
    }

  private def applyTransformCase(
    root: DynamicValue,
    action: MigrationAction.TransformCase
  ): Either[SchemaError, DynamicValue] =
    navigateAndModify(root, action.at) {
      case variant: DynamicValue.Variant =>
        if (variant.caseNameValue == action.caseName)
          action.migration(variant.value).map(v => DynamicValue.Variant(variant.caseNameValue, v))
        else
          Right(variant)
      case _ =>
        Left(SchemaError.message(s"TransformCase: expected Variant at path", action.at))
    }

  // ── Collection Operations ─────────────────────────────────────────────

  private def applyTransformElements(
    root: DynamicValue,
    action: MigrationAction.TransformElements
  ): Either[SchemaError, DynamicValue] =
    navigateAndModify(root, action.at) {
      case seq: DynamicValue.Sequence =>
        val elems  = seq.elements
        val cb     = ChunkBuilder.make[DynamicValue](elems.length)
        var idx    = 0
        var error: Either[SchemaError, DynamicValue] = null
        while (idx < elems.length && error == null) {
          action.transform.eval(elems(idx)) match {
            case Right(v)  => cb += v
            case Left(err) => error = Left(err)
          }
          idx += 1
        }
        if (error != null) error else Right(DynamicValue.Sequence(cb.result()))
      case _ =>
        Left(SchemaError.message(s"TransformElements: expected Sequence at path", action.at))
    }

  private def applyTransformKeys(
    root: DynamicValue,
    action: MigrationAction.TransformKeys
  ): Either[SchemaError, DynamicValue] =
    navigateAndModify(root, action.at) {
      case map: DynamicValue.Map =>
        val entries = map.entries
        val cb      = ChunkBuilder.make[(DynamicValue, DynamicValue)](entries.length)
        var idx     = 0
        var error: Either[SchemaError, DynamicValue] = null
        while (idx < entries.length && error == null) {
          val (k, v) = entries(idx)
          action.transform.eval(k) match {
            case Right(newK) => cb += ((newK, v))
            case Left(err)   => error = Left(err)
          }
          idx += 1
        }
        if (error != null) error else Right(DynamicValue.Map(cb.result()))
      case _ =>
        Left(SchemaError.message(s"TransformKeys: expected Map at path", action.at))
    }

  private def applyTransformValues(
    root: DynamicValue,
    action: MigrationAction.TransformValues
  ): Either[SchemaError, DynamicValue] =
    navigateAndModify(root, action.at) {
      case map: DynamicValue.Map =>
        val entries = map.entries
        val cb      = ChunkBuilder.make[(DynamicValue, DynamicValue)](entries.length)
        var idx     = 0
        var error: Either[SchemaError, DynamicValue] = null
        while (idx < entries.length && error == null) {
          val (k, v) = entries(idx)
          action.transform.eval(v) match {
            case Right(newV) => cb += ((k, newV))
            case Left(err)   => error = Left(err)
          }
          idx += 1
        }
        if (error != null) error else Right(DynamicValue.Map(cb.result()))
      case _ =>
        Left(SchemaError.message(s"TransformValues: expected Map at path", action.at))
    }

  // ── Nested Migration ──────────────────────────────────────────────────

  private def applyNestedMigration(
    root: DynamicValue,
    action: MigrationAction.ApplyMigration
  ): Either[SchemaError, DynamicValue] =
    navigateAndModify(root, action.at) { target =>
      action.migration(target)
    }

  // ── Path Navigation ───────────────────────────────────────────────────

  /**
   * Navigates to the value at the given path, applies the modification
   * function, and reconstructs the document with the modified value.
   *
   * For the root path (empty nodes), applies the function directly.
   * For nested paths, recursively descends into records and variants.
   */
  private def navigateAndModify(
    root: DynamicValue,
    path: DynamicOptic
  )(
    f: DynamicValue => Either[SchemaError, DynamicValue]
  ): Either[SchemaError, DynamicValue] = {
    val nodes = path.nodes
    if (nodes.isEmpty) f(root)
    else navigateAndModifyAt(root, nodes, 0, f)
  }

  private def navigateAndModifyAt(
    current: DynamicValue,
    nodes: IndexedSeq[DynamicOptic.Node],
    nodeIdx: Int,
    f: DynamicValue => Either[SchemaError, DynamicValue]
  ): Either[SchemaError, DynamicValue] = {
    if (nodeIdx >= nodes.length) f(current)
    else nodes(nodeIdx) match {
      case fieldNode: DynamicOptic.Node.Field =>
        current match {
          case record: DynamicValue.Record =>
            val fields = record.fields
            var found  = false
            val newFields = ChunkBuilder.make[(String, DynamicValue)](fields.length)
            var idx    = 0
            var error: Either[SchemaError, DynamicValue] = null
            while (idx < fields.length && error == null) {
              val (name, v) = fields(idx)
              if (name == fieldNode.name) {
                found = true
                navigateAndModifyAt(v, nodes, nodeIdx + 1, f) match {
                  case Right(updated) => newFields += ((name, updated))
                  case left           => error = left
                }
              } else {
                newFields += ((name, v))
              }
              idx += 1
            }
            if (error != null) error
            else if (!found)
              Left(SchemaError.message(
                s"Path navigation: field '${fieldNode.name}' not found",
                new DynamicOptic(nodes.take(nodeIdx + 1))
              ))
            else
              Right(new DynamicValue.Record(newFields.result()))
          case _ =>
            Left(SchemaError.message(
              s"Path navigation: expected Record for field '${fieldNode.name}'",
              new DynamicOptic(nodes.take(nodeIdx + 1))
            ))
        }

      case caseNode: DynamicOptic.Node.Case =>
        current match {
          case variant: DynamicValue.Variant =>
            if (variant.caseNameValue == caseNode.name)
              navigateAndModifyAt(variant.value, nodes, nodeIdx + 1, f).map { updated =>
                DynamicValue.Variant(variant.caseNameValue, updated)
              }
            else
              Right(current) // Not this case, leave unchanged
          case _ =>
            Left(SchemaError.message(
              s"Path navigation: expected Variant for case '${caseNode.name}'",
              new DynamicOptic(nodes.take(nodeIdx + 1))
            ))
        }

      case idxNode: DynamicOptic.Node.AtIndex =>
        current match {
          case seq: DynamicValue.Sequence =>
            val elems = seq.elements
            if (idxNode.index < 0 || idxNode.index >= elems.length)
              Left(SchemaError.message(
                s"Path navigation: index ${idxNode.index} out of bounds (size=${elems.length})",
                new DynamicOptic(nodes.take(nodeIdx + 1))
              ))
            else
              navigateAndModifyAt(elems(idxNode.index), nodes, nodeIdx + 1, f).map { updated =>
                DynamicValue.Sequence(elems.updated(idxNode.index, updated))
              }
          case _ =>
            Left(SchemaError.message(
              s"Path navigation: expected Sequence for index access",
              new DynamicOptic(nodes.take(nodeIdx + 1))
            ))
        }

      case _: DynamicOptic.Node.Elements.type =>
        current match {
          case seq: DynamicValue.Sequence =>
            val elems = seq.elements
            val cb    = ChunkBuilder.make[DynamicValue](elems.length)
            var idx   = 0
            var error: Either[SchemaError, DynamicValue] = null
            while (idx < elems.length && error == null) {
              navigateAndModifyAt(elems(idx), nodes, nodeIdx + 1, f) match {
                case Right(updated) => cb += updated
                case left           => error = left
              }
              idx += 1
            }
            if (error != null) error else Right(DynamicValue.Sequence(cb.result()))
          case _ =>
            Left(SchemaError.message(
              "Path navigation: expected Sequence for elements traversal",
              new DynamicOptic(nodes.take(nodeIdx + 1))
            ))
        }

      case _: DynamicOptic.Node.MapValues.type =>
        current match {
          case map: DynamicValue.Map =>
            val entries = map.entries
            val cb      = ChunkBuilder.make[(DynamicValue, DynamicValue)](entries.length)
            var idx     = 0
            var error: Either[SchemaError, DynamicValue] = null
            while (idx < entries.length && error == null) {
              val (k, v) = entries(idx)
              navigateAndModifyAt(v, nodes, nodeIdx + 1, f) match {
                case Right(updated) => cb += ((k, updated))
                case left           => error = left
              }
              idx += 1
            }
            if (error != null) error else Right(DynamicValue.Map(cb.result()))
          case _ =>
            Left(SchemaError.message(
              "Path navigation: expected Map for values traversal",
              new DynamicOptic(nodes.take(nodeIdx + 1))
            ))
        }

      case _: DynamicOptic.Node.MapKeys.type =>
        current match {
          case map: DynamicValue.Map =>
            val entries = map.entries
            val cb      = ChunkBuilder.make[(DynamicValue, DynamicValue)](entries.length)
            var idx     = 0
            var error: Either[SchemaError, DynamicValue] = null
            while (idx < entries.length && error == null) {
              val (k, v) = entries(idx)
              navigateAndModifyAt(k, nodes, nodeIdx + 1, f) match {
                case Right(updated) => cb += ((updated, v))
                case left           => error = left
              }
              idx += 1
            }
            if (error != null) error else Right(DynamicValue.Map(cb.result()))
          case _ =>
            Left(SchemaError.message(
              "Path navigation: expected Map for keys traversal",
              new DynamicOptic(nodes.take(nodeIdx + 1))
            ))
        }

      case _: DynamicOptic.Node.Wrapped.type =>
        // Wrapper types in DynamicValue are represented transparently
        navigateAndModifyAt(current, nodes, nodeIdx + 1, f)

      case other =>
        Left(SchemaError.message(
          s"Path navigation: unsupported node type ${other.getClass.getSimpleName}",
          new DynamicOptic(nodes.take(nodeIdx + 1))
        ))
    }
  }
}
