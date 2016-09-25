package co.blocke.scalajack
package typeadapter
package javatime

import java.time.Duration

object DurationTypeAdapter extends SimpleTypeAdapter[Duration] {

  override def read(reader: Reader): Duration =
    reader.peek match {
      case TokenType.Null ⇒
        reader.readNull()

      case TokenType.String ⇒
        try {
          Duration.parse(reader.readString())
        } catch {
          case dtpe: java.time.format.DateTimeParseException ⇒ throw new java.time.format.DateTimeParseException(dtpe.getMessage + "\n" + reader.showError(), dtpe.getParsedString, dtpe.getErrorIndex)
        }

      case actual ⇒ {
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading Duration value.  (Is your value wrapped in quotes?)\n" + reader.showError())
      }
    }

  override def write(value: Duration, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }

}