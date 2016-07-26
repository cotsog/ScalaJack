package co.blocke
package scalajack

import scala.collection.mutable.LinkedHashMap

/**
 * Deferred objects defer their parsing until later, due to type erasure, for example when a trait's
 * materialized runtime value is a parameterized class.
 *
 * The JSON parser will parse a deferred object will parse JSON content to a map of key/value pairs 
 * (the blob) and this BlobParser resolves the map into an actual (typed) value, i.e. materializes
 * the class, presumably when the runtime type is known and specified for a proper type cast.
 * 
 *  SEE THIS: http://stackoverflow.com/questions/32246734/scala-pattern-match-a-function-how-to-get-around-type-erasure
 *
 */
object BlobParser {
	// def parse( t:AType, blob:Map[String,Any] ) = {
	def parse( t:AType, blob:Any, vctx:VisitorContext, topLevel:Boolean = false ):Any = {
		t match {
			case bt:CCType =>
				val fixedBlob = blob.asInstanceOf[Map[String,Any]]
				bt.materialize( bt.members.map{ case(name,(atype,defaultVal)) =>
					(name, parse(atype, fixedBlob(name), vctx))
				}.toMap)
			case bt:TraitType =>
				val fixedBlob = blob.asInstanceOf[Map[String,Any]]
				val sjCC = {
					// Look-ahead and find type hint--figure out what kind of object his is and inspect it.
					val hintClass = fixedBlob.get(vctx.hintMap.getOrElse(bt.name,vctx.hintMap("default")))
						// See if we need to look up actual objClass (e.g. abbreviation) or if its ready-to-eat
						.map( candidate => vctx.hintValueRead.get(bt.name).map(_(candidate.toString)).getOrElse(candidate.toString) )
					if( !hintClass.isDefined )
						throw new json.JsonParseException(s"No type hint given for trait ${bt.name}",0)
					val sjObjType = Analyzer.inspectByName(hintClass.get.toString,Some(bt))
					if( !sjObjType.isInstanceOf[CCType] ) throw new json.JsonParseException(s"Unknown case class type in trait hint: ${hintClass.get}.  You may need to prime with ScalaJack.primeDeferred().",0)
					sjObjType.asInstanceOf[CCType]
				}
				sjCC.materialize(fixedBlob)
			case bt:PrimType => blob
			case bt:EnumType => bt.enum.withName( blob.toString )
			case bt:CollType =>
				if( bt.isOptional ) {
					Some( parse(bt.colTypes(0), blob, vctx) )
				} else if( bt.collCode > 0 && bt.collCode < 10 ) {  // range in PrimitiveTypes for Map variants
					val fixedBlob = blob.asInstanceOf[Map[Any,Any]]
					val mapAcc = fixedBlob.map{ case(k,v) => 
						bt.colTypes(0) match { // For canonical JSON Map key must resolve to String type.  Anything goes for non-canoical.
							case ct if(!vctx.isCanonical) =>
							case ct:PrimType if(ct.primCode == PrimitiveTypes.STRING) =>
							case et:EnumType => 
							case ValueClassType(_,PrimType("String"),_,_) | ValueClassType(_,PrimType("java.lang.String"),_,_) | ValueClassType(_,EnumType(_,_),_,_) =>
							case t => throw new json.JsonParseException("Map keys must be of type String in canonical JSON",0)
						}
						(parse(bt.colTypes(0),k,vctx),parse(bt.colTypes(1),v,vctx))
					}
					PrimitiveTypes.collTypes(bt.collCode)(mapAcc.toList)
				} else if(bt.collCode > 29) {  // range in PrimitiveTypes for Tuple variants
					val arity = """\d+""".r.findFirstIn(bt.name).get.toInt
					val fixedBlob = blob.asInstanceOf[List[Any]]
					val tv = (0 to arity-1).map( a => parse(bt.colTypes(a),fixedBlob(a),vctx) ) // parse tuple values
					arity match {
						case 2  => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1)) )
						case 3  => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2)) )
						case 4  => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3)) )
						case 5  => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4)) )
						case 6  => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5)) )
						case 7  => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6)) )
						case 8  => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7)) )
						case 9  => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8)) )
						case 10 => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9)) )
						case 11 => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10)) )
						case 12 => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11)) )
						case 13 => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12)) )
						case 14 => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13)) )
						case 15 => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13),tv(14)) )
						case 16 => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13),tv(14),tv(15)) )
						case 17 => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13),tv(14),tv(15),tv(16)) )
						case 18 => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13),tv(14),tv(15),tv(16),tv(17)) )
						case 19 => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13),tv(14),tv(15),tv(16),tv(17),tv(18)) )
						case 20 => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13),tv(14),tv(15),tv(16),tv(17),tv(18),tv(19)) )
						case 21 => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13),tv(14),tv(15),tv(16),tv(17),tv(18),tv(19),tv(20)) )
						case 22 => PrimitiveTypes.collTypes(bt.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13),tv(14),tv(15),tv(16),tv(17),tv(18),tv(19),tv(20),tv(21)) )
					}
				} else {
					val fixedBlob = blob.asInstanceOf[List[Any]]
					PrimitiveTypes.collTypes(bt.collCode)(fixedBlob.map( parse(bt.colTypes(0), _, vctx) ))
				}
			case bt:ValueClassType =>
				if( bt.isTypeParam || topLevel ) 
					parseValueClass(bt, parseValueClassPrimitive(bt, blob, vctx).asInstanceOf[AnyRef])
				else
					parseValueClassPrimitive(bt, blob, vctx)
			// case sj:DeferredType => // Nested Deferred not supported at this time
			// case bt:CustomType => // Custom types for Deferred not supported at this time
		}
	}

	private def parseValueClassPrimitive( vc:ValueClassType, blob:Any, vctx:VisitorContext ) = 
		vctx.valClassHandlers.get("default").flatMap(_.get(vc.name).map( handler => 
			handler.read( parse(PrimType("String"), blob.asInstanceOf[AnyRef], vctx ).asInstanceOf[AnyRef] )
		)).orElse( Some( parse(vc.vcType, blob, vctx).asInstanceOf[AnyRef]) ).get
	private def parseValueClass( vc:ValueClassType, primitive:AnyRef ) = Class.forName(vc.name).getConstructors()(0).newInstance(primitive)
}