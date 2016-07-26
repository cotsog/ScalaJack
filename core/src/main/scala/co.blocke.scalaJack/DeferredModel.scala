package co.blocke
package scalajack

import scala.reflect.runtime.universe._

case class DeferredType(
	name         : String
) extends AType {
	def dup = this.copy() 
}

trait Deferred[A] {
	def resolve(vctx:VisitorContext = VisitorContext())(implicit at:TypeTag[A]):Option[Resolved] = None // placeholder for read...don't use directly
	def become[T](vctx:VisitorContext = VisitorContext())(implicit at:TypeTag[A], tt:TypeTag[T]):Option[T] = None 
}

case class DeferredInstance[A](  // used for rendering only
	traitType    : TraitType,
	specificType : CCType,
	wrapped      : A
) extends Deferred[A]

case class DeferredBlob[A](
	blob : Map[String,Any]
) extends Deferred[A] {
	private val pattern = "(.*?)\\[(.*)\\]".r  // pattern for parameterized type hint

	private def fixHint(hint:String,vctx:VisitorContext,keyName:String) = 
		pattern.findAllIn(hint).matchData.map{ m =>
			val subtypes = m.group(2).split(',').map{ boxed => 
				vctx.hintValueRead.get(keyName).map(_(boxed)).getOrElse(boxed)
				}
			vctx.hintValueRead.get(keyName).map(_(m.group(1))).getOrElse(m.group(1)) + '['+subtypes.mkString(",")+']'
		}.toList.headOption.get

	override def resolve(vctx:VisitorContext = VisitorContext())(implicit at:TypeTag[A]):Option[Resolved] = 
		blob.get( vctx.hintMap.getOrElse(at.tpe.typeSymbol.fullName,vctx.hintMap("default"))).flatMap{ hint =>
			val fixedHint = fixHint(hint.toString,vctx,at.tpe.typeSymbol.fullName)
			Analyzer.inspectByName(fixedHint)(at) match {
				case ccType:CCType => Some(Resolved(ccType.typetag,BlobParser.parse(ccType,blob,vctx,true).asInstanceOf[A]))
				case _ => None
			}
		}

	// Becomes the specific, statically-typed thing specified by T
	override def become[T](vctx:VisitorContext = VisitorContext())(implicit at:TypeTag[A], tt:TypeTag[T]):Option[T] = 
		resolve( vctx ).map(_.uncasted.asInstanceOf[T])
}

case class Resolved( typetag:Type, uncasted:Any )

object Deferred {

	def apply[A,T](o:T)(implicit at:TypeTag[A],tt:TypeTag[T]) = {
		val traitType = Analyzer.inspect(o.asInstanceOf[A])(at).asInstanceOf[TraitType]
		DeferredInstance[A](
			traitType, 
			Analyzer.inspect(o)(tt).asInstanceOf[CCType].copy(superTrait = Some(traitType)),
			o.asInstanceOf[A])
	}

	def prime[T](fn: Any=>T)(implicit tt:TypeTag[T]) =
		Analyzer.inspectByName(tt.tpe.typeSymbol.fullName, None)
}