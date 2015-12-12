package gather.ir

import java.io.{File => Path}
import java.net.URL
import java.time.Duration

/** Type */
sealed abstract class Type
/*
 * TODO: Allow more complex recurring units somehow (e.g., MWF 6am); this would
 *       cause both backend problems for representation and frontend problems
 *       writing in the language and is not a priority. 
 */
case object StringType extends Type
case object URLType extends Type
case object FileType extends Type
case class SetType(ty: Type) extends Type

/** Values */
sealed abstract class Value(ty: Type)
case class StringValue(s: String) extends Value(StringType)
case class FileValue(path: Path) extends Value(FileType)
case class URLValue(address: String, updateFreq: Duration) extends Value(URLType)

/** Expression */
sealed abstract class Expr
case class StringLiteral(s: StringValue) extends Expr
case class FileLiteral(path: FileValue) extends Expr
case class URLLiteral(url: URLValue) extends Expr
case class SetExpr(elems: Set[Expr]) extends Expr
case class VariableExpr(v: Symbol) extends Expr

/** Actions */
sealed abstract class Action
case class ShellAction(shellCommand: Seq[Expr]) extends Action
// TODO: Builtins, other modules

/** Assignment */
sealed abstract class Assignment
case class SimpleAssignment(v: Symbol, e: Expr) extends Assignment
case class ForeachAssignment(v: Symbol, e: Expr) extends Assignment

/** Statements*/
sealed abstract class Statement
case class Declaration(ty: Type, v: Symbol, initialVal: Expr) extends Statement
case class Rule(in: List[Assignment], out: List[Assignment], action: Action) extends Statement
