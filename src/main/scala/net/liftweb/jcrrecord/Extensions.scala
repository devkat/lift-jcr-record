package net.liftweb.jcrrecord

import javax.jcr.Node
import javax.jcr.Property
import javax.jcr.NodeIterator
import javax.jcr.PropertyIterator
import net.liftweb.record.field.Countries
import Option.{apply => ?}
import java.util.Calendar
import org.apache.commons.io.IOUtils
import javax.jcr.Value

object Extensions {
  
  import JcrRecord._
  
  implicit def extendNode(node: Node) = new NodeExtender(node)
  
  implicit def toIterator(it:NodeIterator) = new Iterator[Node] {
    def hasNext = it.hasNext
    def next = it.nextNode
  }

  implicit def toIterator(it:PropertyIterator) = new Iterator[Property] {
    def hasNext = it.hasNext
    def next = it.nextProperty
  }
  
  def factory = jcrSession.getValueFactory
  
  implicit def propertyToString(p:Property): String = p.getString
  implicit def propertyToStringOption(p:Property): Option[String] = ?(p.getString)
  implicit def stringToValue(s:String): Value = factory.createValue(s)
  implicit def stringOptionToValue(s:Option[String]) = factory.createValue(s.orNull)
  
  implicit def propertyToBoolean(p:Property): Boolean = p.getBoolean
  implicit def propertyToBooleanOption(p:Property): Option[Boolean] = ?(p.getBoolean)
  
  def country(l:Long) = Countries(l.asInstanceOf[Int])
  implicit def propertyToCountry(p:Property): Countries.Value = country(p.getLong)
  implicit def propertyToCountryOption(p:Property): Option[Countries.Value] = ?(p.getLong) map country _
  
  implicit def propertyToLong(p:Property): Long = p.getLong
  implicit def propertyToLongOption(p:Property): Option[Long] = ?(p.getLong)
  
  implicit def propertyToCal(p:Property): Calendar = p.getDate
  implicit def propertyToCalOption(p:Property): Option[Calendar] = ?(p.getDate)
  
  implicit def propertyToDecimal(p:Property): BigDecimal = p.getLong
  implicit def propertyToDecimalOption(p:Property): Option[BigDecimal] = ?(p.getDecimal)

  implicit def propertyToBinary(p:Property): Array[Byte] = IOUtils.toByteArray(p.getBinary.getStream)
  implicit def propertyToBinaryOption(p:Property): Option[Array[Byte]] = ?(IOUtils.toByteArray(p.getBinary.getStream))

  class NodeExtender(node: Node) {

    def childNodes = new Iterator[Node] {
      val it = node.getNodes
      def hasNext = it.hasNext
      def next = it.nextNode
    }
    
    def properties = new Iterator[Property] {
      val it = node.getProperties
      def hasNext = it.hasNext
      def next = it.nextProperty
    }
  }
}
