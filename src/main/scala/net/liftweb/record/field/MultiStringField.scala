package net.liftweb.record.field

import net.liftweb.record.Record
import net.liftweb.record.Field
import net.liftweb.record.MandatoryTypedField
import net.liftweb.json.JValue
import net.liftweb.common._
import net.liftweb.http.js._
import JE._
import net.liftweb.json.JString
import net.liftweb.record.FieldHelpers

class MultiStringField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[List[String], OwnerType] with MandatoryTypedField[List[String]] with ListTypedField[String] {
  
  override val listManifest = manifest[List[String]]
  
  def owner = rec

  def this(rec: OwnerType, value: List[String]) = {
    this(rec)
    set(value)
  }

  def itemAsJs(item:String): JsExp = Str(item)
  
  def itemAsJValue(item:String): JValue = JString(item)
  
  def itemFromJValue(v:JValue): Box[String] = v match {
    case JString(s) => Full(s)
    case other => FieldHelpers.expectedA("JString", other)
  }

}
