package net.liftweb.jcrrecord

import net.liftweb.record.Record
import net.liftweb.record.KeyedRecord
import net.liftweb.record.MetaRecord
import net.liftweb.record.field._
import java.math.MathContext
import org.jcrom.annotations.JcrName

/**
 * Test Record: Company. It has many different field types for test purposes.
 */
class Company private () extends Record[Company] with NodeRecord[Company] {

  override def meta = Company

  //override val idField = new LongField(this)
  
  def jcrName = nodeName.get

  val nodeName = new StringField(this, 256)
  val name = new StringField(this, 256)
  val description = new OptionalTextareaField(this, 1000)
  val country = new CountryField(this)
  val postCode = new PostalCodeField(this, country)
  val created = new DateTimeField(this)
  val employeeSatisfaction = new OptionalDecimalField(this, new MathContext(10), 5)

  //lazy val employees = MySchema.companyToEmployees.left(this)

}

object Company extends Company with MetaRecord[Company] {

}

object CompanySchema extends Schema(
  classOf[Company]
)

