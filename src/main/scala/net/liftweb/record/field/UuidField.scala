package net.liftweb {
package record {
package field {

import scala.xml._
import net.liftweb.common._
import net.liftweb.http.S
import net.liftweb.json.JsonAST.{JNothing, JNull, JValue, JString}
import net.liftweb.util._
import Helpers._
import S._
import java.util.UUID
import net.liftweb.http.js._
import JE._

trait UuidTypedField extends TypedField[UUID] {
  
  def classOfPersistentField = classOf[UUID]
  
  def setFromAny(in: Any): Box[UUID] = in match {
    case seq: Seq[_] if !seq.isEmpty => setFromAny(seq.head)
    case _ => genericSetFromAny(in)
  }

  def setFromString(s: String): Box[UUID] = s match {
    case "" if optional_? => setBox(Empty)
    case _ => setBox(tryo(UUID.fromString(s)))
  }

  def defaultValue = UUID.randomUUID()
  
  def asJs = valueBox.map(uuid => Str(uuid.toString)) openOr JsNull

  def asJValue: JValue = valueBox.map(uuid => JString(uuid.toString)) openOr (JNothing: JValue)
  
  def setFromJValue(jvalue: JValue): Box[UUID] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JString(s) => setBox(Full(UUID.fromString(s)))
    case other => setBox(FieldHelpers.expectedA("JString", other))
  }
  

  private def elem = S.fmapFunc(SFuncHolder(this.setFromAny(_))) {
    funcName =>
      <input type="text" maxlength="36"
        name={funcName}
        value={valueBox.map(_.toString) openOr ""}
        tabindex={tabIndex toString}/>
  }
  
  def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> (id + "_field")))
      case _ => Full(elem)
    }
}

class UuidField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[UUID, OwnerType] with MandatoryTypedField[UUID] with UuidTypedField {

  def owner = rec

  def this(rec: OwnerType, value: UUID) = {
    this(rec)
    set(value)
  }
}

class OptionalUuidField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[UUID, OwnerType] with OptionalTypedField[UUID] with UuidTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Box[UUID]) = {
    this(rec)
    setBox(value)
  }
}

class MultipleUuidField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[List[UUID], OwnerType] with MandatoryTypedField[List[UUID]] with ListTypedField[UUID] {
  
  override val listManifest = manifest[List[UUID]]
  
		  /*
  override type ValueType = List[UUID]
  override type MyType = List[UUID]
  */
  
  def owner = rec

  def this(rec: OwnerType, value: List[UUID]) = {
    this(rec)
    set(value)
  }

  def itemAsJs(item:UUID): JsExp = Str(item.toString)
  
  def itemAsJValue(item:UUID): JValue = JString(item.toString)
  
  def itemFromJValue(v:JValue): Box[UUID] = v match {
    case JString(s) => Full(UUID.fromString(s))
    case other => FieldHelpers.expectedA("JString", other)
  }

}

}
}
}