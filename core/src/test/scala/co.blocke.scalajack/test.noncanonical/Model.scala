package co.blocke.scalajack
package test
package noncanonical

import java.util.UUID
import java.lang.{ Boolean => JBoolean, Byte => JByte, Character => JChar, Double => JDouble, Float => JFloat, Integer => JInteger, Long => JLong, Number => JNumber, Short => JShort }
import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInteger }
import java.time._

object Size extends Enumeration {
  val Small, Medium, Large = Value
}

// === Scala Primitive Keys
case class SampleBigDecimal(m: Map[BigDecimal, BigDecimal])
case class SampleBigInt(m: Map[BigInt, BigInt])
case class SampleBoolean(m: Map[Boolean, Boolean])
case class SampleByte(m: Map[Byte, Byte])
case class SampleChar(m: Map[Char, Char])
case class SampleDouble(m: Map[Double, Double])
case class SampleEnumeration(m: Map[Size.Value, Size.Value])
case class SampleFloat(m: Map[Float, Float])
case class SampleInt(m: Map[Int, Int])
case class SampleLong(m: Map[Long, Long])
case class SampleShort(m: Map[Short, Short])

// === Java Primitive Keys
case class SampleJBigDecimal(m: Map[JBigDecimal, JBigDecimal])
case class SampleJBigInteger(m: Map[JBigInteger, JBigInteger])
case class SampleJBoolean(m: Map[JBoolean, JBoolean])
case class SampleJByte(m: Map[JByte, JByte])
case class SampleJChar(m: Map[JChar, JChar])
case class SampleJDouble(m: Map[JDouble, JDouble])
case class SampleJFloat(m: Map[JFloat, JFloat])
case class SampleJInteger(m: Map[JInteger, JInteger])
case class SampleJLong(m: Map[JLong, JLong])
case class SampleJNumber(m: Map[JNumber, JNumber])
case class SampleJShort(m: Map[JShort, JShort])

// === Java Time Keys
case class SampleDuration(m: Map[Duration, Duration])
case class SampleInstant(m: Map[Instant, Instant])
case class SampleLocalDateTime(m: Map[LocalDateTime, LocalDateTime])
case class SampleLocalDate(m: Map[LocalDate, LocalDate])
case class SampleLocalTime(m: Map[LocalTime, LocalTime])
case class SampleOffsetDateTime(m: Map[OffsetDateTime, OffsetDateTime])
case class SampleOffsetTime(m: Map[OffsetTime, OffsetTime])
case class SamplePeriod(m: Map[Period, Period])
case class SampleZonedDateTime(m: Map[ZonedDateTime, ZonedDateTime])

// === Any primitives
case class AnyShell(m: Map[Any, Any])

// === Class Keys
case class SimpleClass(name: String, age: Int, isOk: Boolean, favorite: Any)
case class SampleSimple(m: Map[SimpleClass, SimpleClass])
case class ComplexClass(id: UUID, simple: SimpleClass, allDone: Boolean)
case class SampleComplex(m: Map[ComplexClass, ComplexClass])

object Food extends Enumeration {
  val Seeds, Meat, Pellets, Veggies = Value
}
trait Pet {
  val name: String
  val food: Food.Value
}
case class FishPet(name: String, food: Food.Value, waterTemp: Double) extends Pet
case class DogPet(name: String, food: Food.Value, numLegs: Int) extends Pet
case class CompoundPet(name: String, food: Food.Value, pet: Pet) extends Pet
trait PetHolder {
  val address: String
  val pet: Pet
}
case class ShinyPetHolder(address: String, pet: Pet) extends PetHolder
case class SamplePet(m: Map[Pet, Pet])
case class PolyClass(lookup: Map[String, Int], favs: List[String])
case class SamplePolyClass(m: Map[PolyClass, PolyClass])
case class SampleShiny(m: Map[PetHolder, PetHolder])
case class NCKey(nc: Map[Int, Boolean], name: String)
case class SampleNCKey(m: Map[NCKey, NCKey])