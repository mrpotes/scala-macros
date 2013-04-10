import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.collection.JavaConversions.enumerationAsScalaIterator
import scala.util.matching._
import scala.collection.mutable.ListBuffer

object Macro {
  
  val tomPattern = "^/tom/(.+)".r
  val dickPattern = "^/dick/(.+)".r
  val harryPattern = "^/harry/(.+)".r
  
  def caseStatement(path : String) : String = {
    path match {
      case tomPattern(c) => "Tom says "+c
      case dickPattern(c) => "Dick says "+c
      case harryPattern(c) => "Harry says "+c
      case _ => "Error"
    }
  }
  
  def macroCase(method : String, path : String): String = macro macroCaseImpl
  
  def macroCaseImpl(c : Context)(method : c.Expr[String], path : c.Expr[String]) : c.Expr[String] = {
    import c.universe._
    import c.{universe => u}
    def classFinder(t : Tree) = t match {
      case ClassDef(m,_,_,_) => m.hasFlag(Flag.CASE)
      case _ => false
    }
    def compObjectFinder(t : Tree, name : String) = t match {
      case ModuleDef(_,n,_) => n.decoded.equals(name)
      case _ => false
    }
    def checkCaseClassWithCompanionObject(t : Tree) = {
      val caseClass = t.find(classFinder)
      caseClass.isDefined && {
        val compObj = t.find(compObjectFinder(_, caseClass.get.asInstanceOf[ClassDef].name.decoded))
        compObj.isDefined
      }
    }
    def isOurTrait(traitName : String, imports : List[String]) = {
      isFullyQualifiedOrImported(traitName, "restapi.Read", imports) || 
      isFullyQualifiedOrImported(traitName, "restapi.Write", imports) || 
      isFullyQualifiedOrImported(traitName, "restapi.Delete", imports)
    }
    def isFullyQualifiedOrImported(name : String, expected : String, imports : List[String]) = {
      name.equals(expected) || {
        val expectedSplit = expected.splitAt(expected.lastIndexOf('.') + 1)
        (name.equals(expectedSplit._2) && (imports.contains(expected) || imports.contains(expectedSplit._1 + "_")))
      }
    }
    def importFinder(t : Tree) = t match {
      case Import(Ident(s), _) => s.decoded.startsWith("restapi")
      case _ => false
    }
    def traitFinder(className : String, imports : List[String])(t : Tree) = t match {
      case AppliedTypeTree(Ident(s), List(Ident(n))) => n.decoded.equals(className) && isOurTrait(s.decoded, imports)
      case _ => false
    }
    val modelClasses = c.enclosingRun.units
    		.filter(unit => {
    		  println(showRaw(unit.body))
    		  unit.body.asInstanceOf[PackageDef].name.decoded.equals("model") && checkCaseClassWithCompanionObject(unit.body)
    		})
    		.toList
    
    val regexes = ListBuffer[ValDef]()
    val cases = ListBuffer[CaseDef]()
    modelClasses.foreach(unit => {
      val className = unit.body.children.find(classFinder).get.asInstanceOf[ClassDef].name.decoded
      val freshName = c.fresh("pattern$")
      val regexString = c.Expr[String](Literal(Constant("^/"+className.toLowerCase + "/(.+)")))
      val imports = unit.body.children.filter(importFinder).flatten(unit => {
        val impStmt = unit.asInstanceOf[Import]
        for (sel <- impStmt.selectors if sel.name.equals(nme.WILDCARD) || sel.name.equals(sel.rename)) 
          yield impStmt.expr.asInstanceOf[Ident].name.decoded + "." + sel.name.decoded
      })
      val traits = unit.body.children.find(compObjectFinder(_, className)).get.filter(traitFinder(className, imports) _).map(unit => {
        val traitName = unit.asInstanceOf[AppliedTypeTree].tpt.asInstanceOf[Ident].name.decoded
        if (traitName.contains(".")) traitName.substring(traitName.lastIndexOf('.')+1) else traitName
      })
      println("Class: "+className+" traits "+traits)
      regexes += ValDef(Modifiers(), freshName, TypeTree(), reify(regexString.splice.r).tree)
      val nameIdent = Ident(freshName)
      if (traits.contains("Read")) { 
        cases += CaseDef(
            Apply(Ident("Tuple2"), List(Literal(Constant("GET")), Apply(nameIdent, List(Bind(newTermName("m"), Ident(nme.WILDCARD)))))), 
            EmptyTree, 
            Apply(Select(Literal(Constant(className + " says ")), newTermName("$plus")), List(Ident(newTermName("m")))))
      }
      if (traits.contains("Write")) { 
        cases += CaseDef(
            Apply(Ident("Tuple2"), List(Literal(Constant("POST")), Apply(nameIdent, List(Bind(newTermName("m"), Ident(nme.WILDCARD)))))), 
            EmptyTree, 
            Apply(Select(Literal(Constant(className + " listens ")), newTermName("$plus")), List(Ident(newTermName("m")))))
      }
      if (traits.contains("Delete")) { 
        cases += CaseDef(
            Apply(Ident("Tuple2"), List(Literal(Constant("DELETE")), Apply(nameIdent, List(Bind(newTermName("m"), Ident(nme.WILDCARD)))))), 
            EmptyTree, 
            Apply(Select(Literal(Constant(className + " goes away ")), newTermName("$plus")), List(Ident(newTermName("m")))))
      }
    })
    cases += CaseDef(Ident(nme.WILDCARD), EmptyTree, Literal(Constant("Error")))
    val block = Block(regexes.toList, Match(reify((method.splice, path.splice)).tree, cases.toList))
    println(show(block))
    c.Expr[String](block)
  }
  
}
