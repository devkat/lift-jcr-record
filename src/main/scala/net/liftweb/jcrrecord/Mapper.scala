package net.liftweb.jcrrecord

import javax.jcr.Node
import net.liftweb.record.Record
import net.liftweb.record.Field
import net.liftweb.record.field._
import net.liftweb.record.MandatoryTypedField
import java.util.Calendar
import net.liftweb.record.OptionalTypedField
import javax.jcr.Binary
import javax.jcr.ValueFactory
import java.io.ByteArrayInputStream
import javax.jcr.Value
import org.apache.commons.io.IOUtils
import javax.jcr.PropertyType

trait Mapper[T <: NodeRecord[T]] {
  self: NodeRecord[T] =>

  import JcrRecord._
  import Extensions._

  def load(node: Node) = {
    jcrNode = Some(node)
    allFields foreach { field =>
      print("%s.%s := ".format(node.getPath, field.name))
      if (node.hasProperty(field.name)) {
        loadProperty(field)
        println(" -> " + field.get)
      } else {
        println("[property not found]")
      }
    }
  }

  def loadProperty(field: Field[_, T]) {
    withNode { n =>
      val prop = n.getProperty(field.name)
      val v = if (prop.isMultiple) {
        prop.getValues.toList map value2any _
      } else {
        value2any(prop.getValue)
      }
      print(v)
      field.setFromAny(v)
    }
  }

  def value2any(p: Value) = p.getType match {
    case PropertyType.BINARY => IOUtils.toByteArray(p.getBinary.getStream)
    case PropertyType.BOOLEAN => p.getBoolean
    case PropertyType.DATE => p.getDate
    case PropertyType.DECIMAL => p.getDecimal
    case PropertyType.DOUBLE => p.getDouble
    case PropertyType.LONG => p.getLong
    case PropertyType.NAME => p.getString
    case PropertyType.PATH => p.getString
    case PropertyType.REFERENCE => p.getString
    case PropertyType.STRING => p.getString
    case PropertyType.UNDEFINED => throw new RuntimeException("Undefined property type.")
    case PropertyType.URI => p.getString
    case PropertyType.WEAKREFERENCE => p.getString
    case t => throw new RuntimeException("Unknown property type " + PropertyType.nameFromValue(t))
  }

  def save() {
    allFields filter { !_.ignoreField_? } foreach { f: Field[_, T] => saveProperty(f) }
  }

  def saveProperty(field: Field[_, T]) {
    withNode { n =>
      field match {
        /*
        case f: PostalCodeField[_] => n.setProperty(f.name, f.get)
        case f: StringField[_] => n.setProperty(f.name, f.get)
        case f: OptionalStringField[_] => n.setProperty(f.name, f.get.orNull)
        case f: BooleanField[_] => n.setProperty(f.name, f.get)
        case f: OptionalBooleanField[_] => n.setProperty(f.name, f.get.getOrElse(null).asInstanceOf[Boolean])
        case f: CountryField[_] => n.setProperty(f.name, f.get.id)
        case f: OptionalCountryField[_] => n.setProperty(f.name, f.get.map(_.id).getOrElse(null).asInstanceOf[Int])
        case f: DateTimeField[_] => n.setProperty(f.name, f.get)
        case f: OptionalDecimalField[_] => n.setProperty(f.name, f.get.map(_.toLong).getOrElse(null).asInstanceOf[Long])
        */
        case f: MandatoryTypedField[_] => setProperty(f.name, f.get)
        case f: OptionalTypedField[_] => setProperty(f.name, f.get match {
          case Some(x) => x
          case None => null
        })
        case f => throw new RuntimeException("Unsupported field " + f.name + " = " + f)
      }
    }
  }

  implicit def scalaBigDecimal2javaBigDecimal(i: BigDecimal): java.math.BigDecimal =
    new java.math.BigDecimal(i.toDouble)

  implicit def byteArray2binary(a: Array[Byte]): Binary =
    jcrSession.getValueFactory().createBinary(new ByteArrayInputStream(a))

  def setProperty(name: String, v: Any) = withNode { n =>
    v match {
      case null => n.setProperty(name, null.asInstanceOf[String])
      case l: List[_] => n.setProperty(name, toJcrValues(l))
      case v => n.setProperty(name, toJcrValue(v))
    }
  }

  lazy val factory = jcrSession.getValueFactory

  def toJcrValues[T <: Any](l: List[T]): Array[Value] = l map toJcrValue _ toArray

  def toJcrValue(v: Any): Value = v match {
    case a: Array[Byte] => factory.createValue(a)
    case i: BigDecimal => factory.createValue(i)
    case b: Boolean => factory.createValue(b)
    case c: Calendar => factory.createValue(c)
    case c: Countries.Value => factory.createValue(c.id)
    case d: Double => factory.createValue(d)
    case i: Int => factory.createValue(i)
    case l: Long => factory.createValue(l)
    case s: String => factory.createValue(s, PropertyType.STRING)
    case v => throw new RuntimeException("Unsupported property value " + v)
  }

}