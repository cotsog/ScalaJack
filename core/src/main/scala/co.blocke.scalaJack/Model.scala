package co.blocke.scalajack

import scala.reflect.runtime.universe._

trait SjItem
trait SjType extends SjItem {
	val name : String
}
trait SjParam extends SjItem{
	val fieldName : String
	val ftype : SjType
}

case class SjField( fieldName:String, ftype:SjType ) extends SjParam

// SjTypes -- name == scala class name except for SjTypesymbol where it = the type symbol placeholder
case class SjCaseClass( name:String, fields:List[SjField], isTrait:Boolean = false ) extends SjType
case class SjCaseClassProxy( name:String, params:List[String], fields:List[SjField] ) extends SjType
case class SjTrait( name:String, resolvedParams:Map[String,SjType] = Map.empty[String,SjType] ) extends SjType
case class SjTraitProxy( name:String, params:List[String] ) extends SjType
case class SjCollection( name:String, collectionType:List[SjType] ) extends SjType {
	def isOptional = name == "scala.Option"
}
case class SjPrimitive( name:String ) extends SjType
case class SjTypeSymbol( name:String ) extends SjType
case class SjValueClass( name:String, vcType:SjType, vFieldName:String ) extends SjType
case class SjValueClassProxy( name:String, vcType:SjType, vFieldName:String ) extends SjType
case class SjEnum( name:String, enum:Enumeration ) extends SjType

class ReflectException(msg:String) extends Exception(msg)
