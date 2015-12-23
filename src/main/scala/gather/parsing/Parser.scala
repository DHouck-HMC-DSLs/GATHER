package gather.parsing

import scala.language.postfixOps

import java.io.{File => Path}
import java.net.URL
import java.time.Duration

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.PackratParsers

// For some reason when I say ._ it fails.
import gather.ir.{
	Statement, Declaration, Rule, Action, ShellAction,
	Type, StringType, URLType, FileType, SetType,
	Expr, StringLiteral, URLLiteral, FileLiteral, SetExpr, VariableExpr,
	StringValue, URLValue, FileValue,
	Assignment, SimpleAssignment, ForeachAssignment, AddToAssignment
}

/**
 * @author dhouck
 */
object GatherParser extends JavaTokenParsers with PackratParsers {
  def apply(s: String): ParseResult[Seq[Statement]] = parseAll (module, s)
  
  lazy val module: PackratParser[Seq[Statement]] = phrase(statement +);
  
  lazy val statement: PackratParser[Statement] = rule | variableDeclaration;
  
  // Rules
  lazy val rule: PackratParser[Rule] = (
  	( (rword("in")~":"~>(inputAssignment *))
  	 ~(rword("out")~":"~>(outputAssignment *))
  	 ~(rword("do")~":"~>action)
  	 ) ^^ {case i~o~a => Rule(i,o,a)}
  )
  
  lazy val action: PackratParser[Action] = shellAction
  
  def interpolateShell(s: String): List[Expr] = (
  	// FIXME
  	List(StringLiteral(StringValue(s)))
  )
  
  lazy val shellAction: PackratParser[ShellAction] = (
  	// If you need newlines in your shell escapes, you can make an actual shell script
  	"!"~>"[^\n]+\n".r ^^ {cmd: String => ShellAction(interpolateShell(cmd))}
  )
  
  // Expressions
  lazy val expr: PackratParser[Expr] = (
  	  literal
  	| (name ^^ VariableExpr)
  	| ("{"~>repsep(expr, ",")<~"}" ^^ {es: List[Expr] => SetExpr(es.toSet)})
  )
  
  lazy val literal: PackratParser[Expr] = (
  	  stringLiteral ^^ {s: String => StringLiteral(StringValue(s))}
  	| fileLiteral
  	| urlLiteral
  )
  
  lazy val fileLiteral: PackratParser[FileLiteral] = (
  	// TODO: Get a more intuitive way of representing this
  	// Current method is especially a problem on Windows because of backslash doubling
  	"f"~>stringLiteral ^^ {path: String => FileLiteral(FileValue(new Path(path)))}
  )
  
  lazy val urlLiteral: PackratParser[URLLiteral] = (
  	// TODO: Get a more intuitive way of representing this
  	"u"~>stringLiteral~";"~frequency ^^ {
		case u~";"~f => URLLiteral(URLValue(new URL(u), f))
	}
  )
  
  lazy val frequency: PackratParser[Duration] = (
  	// TODO: More intuitive method for custom periods
  	// TODO: ISO-8601 as parseable by Duration.parse
  	// TODO: Ability to use longer periods (monthly, yearly)  precisely
  	  rword("always") ^^^ Duration.ZERO
  	| rword("secondly)") ^^^ Duration.ofSeconds(1)
  	| rword("minutely") ^^^ Duration.ofMinutes(1)
  	| rword("hourly") ^^^ Duration.ofHours(1)
  	| rword("daily") ^^^ Duration.ofDays(1)
  	| rword("weekly") ^^^ Duration.ofDays(7)
  	| rword("never") ^^^ Duration.ofSeconds(Long.MaxValue) // Close enough
  )
  
  // Variable declaration
  lazy val variableDeclaration: PackratParser[Declaration] = (
  	ty~simpleAssignment ^^ {case t~init => new Declaration(t, init)}
  )
  
  lazy val ty: PackratParser[Type] = (
  	  rword("String") ^^^ StringType
  	| rword("URL") ^^^ URLType
  	| rword("File") ^^^ FileType
  	| rword("SetOf")~>ty ^^ SetType
  )
  
  // Assignments
  lazy val inputAssignment: PackratParser[Assignment] = (
  	simpleAssignment | foreachAssignment
  )
  
  lazy val outputAssignment: PackratParser[Assignment] = (
  	simpleAssignment | addToAssignment
  )
  
  lazy val simpleAssignment: PackratParser[SimpleAssignment] = (
  	name~"="~expr ^^ {case v~"="~e => new SimpleAssignment(v, e)}
  )
  
  lazy val addToAssignment: PackratParser[AddToAssignment] = (
  	name~"+="~expr ^^ {case v~"+="~e => new AddToAssignment(v, e)}
  )
  
  lazy val foreachAssignment: PackratParser[ForeachAssignment] = (
  	rword("foreach")~>simpleAssignment ^^ {
  		case SimpleAssignment(v,e) => ForeachAssignment(v,e)
  	}
  )
  
  // Generic helpers
  def rword(word: String): PackratParser[String] = (
  	ident filter {_ == word} withFailureMessage "Expected reserved word <" + word + ">."
  )
  
  lazy val name: Parser[Symbol] = (
  	ident ^^ Symbol.apply
  )
}
