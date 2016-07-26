package co.blocke.scalajack
package test.v4

import org.scalatest.FunSpec
import org.scalatest.Matchers._
import scala.reflect.runtime.universe._

trait Message
case class MCommand[T](
    payload:T
    ) extends Message
case class MEvent[T](
    payload:T
    ) extends Message
case class MessageRecord(
    id:Int,
    message:Deferred[Message]
){}

case class Bloop(size:Int)  // some payload object for Message classes
case class Floop(x:String)
case class Gloop(b:Boolean)

// Note here that message is (logically) of type Message, which will at runtime 
// actually be a paramerized class, e.g. Command[Foo].  This is typically erased/lost,
// so we need to wrap it in a Deferred object.

class DeferredSpec extends FunSpec {
	val sjJS     = ScalaJack()
	val vc_nc_v  = VisitorContext().copy(isCanonical = false, isValidating = true) 
	val vc_c_v   = VisitorContext().copy(isValidating = true) 
	val vc_nc_nv = VisitorContext().copy(isCanonical = false) 
	// No vc = c_nv canonical (c) and non-validating (nv)

	object JSMaster {
		val a = """{"id":1,"message":{"_hint":"co.blocke.scalajack.test.v4.MEvent[co.blocke.scalajack.test.v4.Bloop]","payload":{"size":99}}}"""
		val b = """{"id":1,"message":{"_hint":"co.blocke.scalajack.test.v4.MCommand[co.blocke.scalajack.test.v4.Floop]","payload":{"x":"Big"}}}"""
		val c = """{"id":1,"message":{"_hint":"co.blocke.scalajack.test.v4.MCommand[co.blocke.scalajack.test.v4.Gloop]","payload":{"b":true}}}"""
	}

	object ScalaMaster {
		val a = MessageRecord(1, Deferred(MEvent(Bloop(99))))
		val b = MessageRecord(1,DeferredBlob(Map("_hint" -> "co.blocke.scalajack.test.v4.MEvent[co.blocke.scalajack.test.v4.Bloop]", "payload" -> Map("size" -> 99))))
		val c = MEvent(Bloop(99))
		val d = MessageRecord(1,DeferredBlob(Map("_hint" -> "co.blocke.scalajack.test.v4.MCommand[co.blocke.scalajack.test.v4.Floop]", "payload" -> Map("x" -> "Big"))))
		val e = MCommand(Floop("Big"))
		val f = MessageRecord(2, Deferred(MCommand(Floop("Quick"))))
		val g = MessageRecord(1,DeferredBlob(Map("_hint" -> "co.blocke.scalajack.test.v4.MCommand[co.blocke.scalajack.test.v4.Gloop]", "payload" -> Map("b" -> true))))
		val h = MCommand(Gloop(true))
	}

	describe("========================\n| -- Deferred Tests -- |\n========================") {
		describe("Basic parsing") {
			it("Render Tests - NC - V") {
				sjJS.render( ScalaMaster.a, vc_nc_v ) should be( JSMaster.a )
			}
			it( "Read Tests - NC - V" ) {
				val step1 = sjJS.read[MessageRecord]( JSMaster.a, vc_nc_v )
				step1 should be( ScalaMaster.b )
				step1.message.become[MEvent[Bloop]]() should be( Some(ScalaMaster.c) )
			}
			it("Render Tests - C - V (should be broken)") {
				sjJS.render( ScalaMaster.a, vc_c_v ) should be( JSMaster.a )
			}
			it( "Read Tests - C - V (should be broken)" ) {
				val step1 = sjJS.read[MessageRecord]( JSMaster.a, vc_c_v )
				step1 should be( ScalaMaster.b )
				step1.message.become[MEvent[Bloop]]() should be( Some(ScalaMaster.c) )
			}
			it("Render Tests - C - NV (should be broken) = no VC") {
				sjJS.render( ScalaMaster.a ) should be( JSMaster.a )
			}
			it( "Read Tests - C - NV (should be broken) = no VC" ) {
				val step1 = sjJS.read[MessageRecord]( JSMaster.a )
				step1 should be( ScalaMaster.b )
				step1.message.become[MEvent[Bloop]]() should be( Some(ScalaMaster.c) )
			}
			it("Render Tests - NC - NV") {
				sjJS.render( ScalaMaster.a, vc_nc_nv ) should be( JSMaster.a )
			}
			it( "Read Tests - NC - NV" ) {
				val step1 = sjJS.read[MessageRecord]( JSMaster.a, vc_nc_nv )
				step1 should be( ScalaMaster.b )
				step1.message.become[MEvent[Bloop]]() should be( Some(ScalaMaster.c) )
			}
		}
		describe("Advanced Parsing") {
			it("Must parse all member types in the blob") {
				pending
			}
			it("Must parse members with parameterized types in the blob") {
				pending
			}
		}
		describe("Priming and Matching") {
			it("Must fail for an un-primed (un-Analyzed) class") {
				val step1 = sjJS.read[MessageRecord]( JSMaster.c )
				step1 should be( ScalaMaster.g )
				step1.message.resolve() should be( None )
			}
			it("Must succeed for the same class after priming") {
				Deferred.prime[MCommand[Gloop]]( o => o.asInstanceOf[MCommand[Gloop]])
				val step1 = sjJS.read[MessageRecord]( JSMaster.c )
				step1 should be( ScalaMaster.g )
				step1.message.resolve().get.uncasted should be( ScalaMaster.h )
			}
			it("Must be able to do unassisted/blind matching on Resolved object") {
				val msgs = List(sjJS.read[MessageRecord]( JSMaster.a ), sjJS.read[MessageRecord]( JSMaster.b ))
				val all = msgs.map( _.message.resolve() ).flatten.map( r => r.typetag match {
					case ty if ty =:= typeOf[MCommand[Floop]] => 
						val x = r.uncasted.asInstanceOf[MCommand[Floop]].payload.x
						s"Floop says $x!"
					case ty if ty =:= typeOf[MEvent[Bloop]] => 
						val x = r.uncasted.asInstanceOf[MEvent[Bloop]].payload.size
						s"Bloop says $x!"
				})
				all should be( List("Bloop says 99!", "Floop says Big!") )
	    	}
		}
	}
}
