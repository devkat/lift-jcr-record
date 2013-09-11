package net.liftweb.jcrrecord

import javax.jcr.Node
import net.liftweb.record.Record
import net.liftweb.record.Field
import net.liftweb.record.field._

trait Mapper[T <: NodeRecord[T]] {
  self: NodeRecord[T] =>
  
  import Extensions._

  def load(node: Node) = {
    jcrNode = Some(node)
    allFields foreach { field =>
      print("set field %s -> ".format(field.name))
      if (node.hasProperty(field.name)) {
        loadProperty(field)
        println(field.get)
      } else {
        println("[property not found]")
      }
    }
  }

  def loadProperty(field: Field[_, T]) {
    withNode { n =>
      val p = n.getProperty(field.name)
      field match {
        case f: PostalCodeField[_] => f.set(p)
        case f: StringField[_] => f.set(p)
        case f: OptionalStringField[_] => f.set(p)
        case f: BooleanField[_] => f.set(p)
        case f: OptionalBooleanField[_] => f.set(p)
        case f: CountryField[_] => f.set(p)
        case f: OptionalCountryField[_] => f.set(p)
        case f: DateTimeField[_] => f.set(p)
        case f: OptionalDecimalField[_] => f.set(p)
        case f => throw new RuntimeException("Unsupported field " + f)
      }
    }
  }

  def save() {
    allFields filter { !_.ignoreField_? } foreach { f: Field[_, T] => saveProperty(f) }
  }

  def saveProperty(field: Field[_, T]) {
    withNode { n =>
      field match {
        case f: PostalCodeField[_] => n.setProperty(f.name, f.get)
        case f: StringField[_] => n.setProperty(f.name, f.get)
        case f: OptionalStringField[_] => n.setProperty(f.name, f.get.orNull)
        case f: BooleanField[_] => n.setProperty(f.name, f.get)
        case f: OptionalBooleanField[_] => n.setProperty(f.name, f.get.getOrElse(null).asInstanceOf[Boolean])
        case f: CountryField[_] => n.setProperty(f.name, f.get.id)
        case f: OptionalCountryField[_] => n.setProperty(f.name, f.get.map(_.id).getOrElse(null).asInstanceOf[Int])
        case f: DateTimeField[_] => n.setProperty(f.name, f.get)
        case f: OptionalDecimalField[_] => n.setProperty(f.name, f.get.map(_.toLong).getOrElse(null).asInstanceOf[Long])
        case f => throw new RuntimeException("Unsupported field " + f)
      }
    }
  }

}