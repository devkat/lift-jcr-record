package net.liftweb.record {
package field {

import net.liftweb.common._
import net.liftweb.http.S
import net.liftweb.http.js._
import JE._
import net.liftweb.json._
import S._
import scala.xml.NodeSeq
import net.liftweb.util._
import Helpers._

trait ListTypedField[T] extends TypedField[List[T]] {
  
  implicit def listManifest: Manifest[List[T]]
		  /*
  
  type ListType = List[T]
  */
  
  /*
   * Unless overridden, MyType is equal to ThisType.  Available for
   * backwards compatibility
   */
  override type MyType = List[T] // For backwards compatibility

  /**
   * ValueType represents the type that users will work with.
  type ValueType = List[T] // For util.BaseField
   */
  
  def classOfPersistentField = classOf[List[T]]
  
  def setFromAny(in: Any): Box[List[T]] = genericSetFromAny(in)
  
  def setFromString(s: String): Box[List[T]] = setBox(Empty)
  
  def defaultValue = Nil
  
  
  def itemAsJs(item:T): JsExp
  
  def itemAsJValue(item:T): JValue
  
  def itemFromJValue(v:JValue): Box[T]

  def asJs = valueBox.map(list => JsArray(list map itemAsJs _)) openOr JsNull

  def asJValue: JValue = valueBox.map(list => JArray(list map itemAsJValue _)) openOr (JNothing: JValue)
  
  def setFromJValue(jvalue: JValue): Box[List[T]] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JArray(s) => {
      val values:List[Box[T]] = s map itemFromJValue _
      val errorVal = values find (v => v.isInstanceOf[Failure])
      setBox(errorVal match {
        case Some(f @ Failure(_, _, _)) => f
        case _ => Full(values map (_.get))
      })
    }
    case other => setBox(FieldHelpers.expectedA("JArray", other))
  }

  private def elem =
    S.fmapFunc(SFuncHolder(this.setFromAny(_))) {
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

  // Members declared in net.liftweb.record.field.ListTypedField
  
  // Members declared in net.liftweb.util.Settable
  //def set(in: List[T]): List[T] = super.set(in)
  
		  /*
  // Members declared in net.liftweb.record.TypedField
  def defaultValueBox: net.liftweb.common.Box[List[java.util.UUID]] = ???
  protected def liftSetFilterToBox(in: net.liftweb.common.Box[List[java.util.UUID]]): net.liftweb.common.Box[List[java.util.UUID]] = ???
  def setFromString(s: String): net.liftweb.common.Box[List[java.util.UUID]] = ???
  protected def toBoxMyType(in: List[java.util.UUID]): net.liftweb.common.Box[List[java.util.UUID]] = ???
  protected def toValueType(in: net.liftweb.common.Box[List[java.util.UUID]]): List[java.util.UUID] = ???
  
  // Members declared in net.liftweb.util.ValueHolder
  def get: List[java.util.UUID] = ???
  def is: List[java.util.UUID] = ???
  */
}

}
}