package co.blocke
package scalajack

import scala.collection.mutable.LinkedHashMap

class ReflectException(msg:String) extends Exception(msg)

trait AType {
	val name    : String
	protected[scalajack] var _isDbKey = false
	def isDbKey : Boolean = _isDbKey
	def dup : AType  // Allows type-safe copying of AType for modifying _isDbKey flag.
	// (A type may be a dbKey in one context but not another so it's not a universal property of a type.)
}

case class CCType(
	name       : String, 
	typetag    : scala.reflect.runtime.universe.Type,
	members    : LinkedHashMap[String,(AType,Option[Any])],   // Map[memberName -> (MemberType,Optional Default Value)]
	paramMap   : LinkedHashMap[String,AType] = LinkedHashMap.empty[String,AType],
	superTrait : Option[TraitType] = None,
	collAnno   : Option[String] = None  // db collumn annotation 
) extends AType {
	private val constructor = Class.forName(name).getConstructors()(0)
	private lazy val defaults = members.collect {
		case (mname, (mtype,mdefault)) if(mdefault.isDefined) => (mname,mdefault.get)
		case (mname, (mtype:CollType,_)) if(mtype.isOptional) => (mname,None)
	}.toMap
	override def toString() = s"[$name -> ${members.map(_._1)}]"
	def dup = this.copy()
	def materialize( content:Map[String,Any] ) = {
		val cv = members.map{ case(mname,_) => content.getOrElse(mname,defaults(mname))}.toArray.asInstanceOf[Array[AnyRef]]
		constructor.newInstance(cv:_*)
	}
}

case class PrimType(name:String) extends AType { 
	val primCode = PrimitiveTypes.primCodes(name)
	def dup = this.copy() 
}

case class CollType(name:String, colTypes:List[AType]) extends AType {
	val collCode = PrimitiveTypes.collCodes(name)
	def isOptional = {collCode == 0}
	def dup = this.copy()
}

case class EnumType(name:String, enum:Enumeration) extends AType { def dup = this.copy() }
case class ValueClassType(name:String, vcType:AType, vFieldName:String, isTypeParam:Boolean) extends AType { def dup = this.copy() }

case class TraitType(
	name     : String, 
	members  : LinkedHashMap[String,AType] = LinkedHashMap.empty[String,AType],
	paramMap : LinkedHashMap[String,AType] = LinkedHashMap.empty[String,AType], 
	default  : Option[Any]=None
) extends AType { 
	def dup = this.copy() 
}

trait CustomType extends AType {
	val readers   : Map[String, (Any => Any)]
	val renderers : Map[String, (Any => Any)]
}

case class ErrType(name:String = "Error") extends AType { 
	def dup = this.copy() 
}

/* Maybe this is ok... Most likely not. 
object HelpMacros {
  import scala.language.experimental.macros
  import reflect.macros.blackbox.Context
  def hello(): Unit = macro hello_impl
  def hello_impl(c: Context)(): c.Expr[Unit] = {
    import c.universe._
    reify { println("Hello World!") }
  }

  // def IsA[T](r:Resolved, variable:String): T = macro IsA_impl
  // def IsA_impl[T:c.WeakTypeTag](c: Context)(r:c.Expr[Resolved], variable:c.Expr[String]): c.Expr[T] = {
  //   import c.universe._
  //   // reify { ty if ty =:= typeOf[c.weakTypeOf[T].toString] => val variable.splice = r.uncasted.asInstanceOf[c.weakTypeOf[T].toString] }
  //   <[ ty if ty =:= typeOf[c.weakTypeOf[T].toString] => val variable.splice = r.uncasted.asInstanceOf[c.weakTypeOf[T].toString] ]>
  // }

  def hi_impl(c: Context)(): c.Expr[Any] = {
  	import c.universe._
  	val q"val $name = $value" = s.tree
	c.Expr(value)
  }  
}
*/