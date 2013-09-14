package net.liftweb.jcrrecord

import net.liftweb.record.Record
import net.liftweb.record.KeyedRecord
import net.liftweb.record.MetaRecord
import net.liftweb.record.field._
import java.math.MathContext
import org.jcrom.annotations.JcrName
import net.liftweb.record._
import net.liftweb.common._
import net.liftweb.json.JValue
import net.liftweb.json.JString
import net.liftweb.http.js.JE.Str
import java.util.Calendar

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

object EmployeeRole extends Enumeration {

  type EmployeeRole = Value

  val Programmer, Manager = Value
}

/**
 * A field type that works just like a String field.
 * Only for testing that custom fields derived from
 * TypedField are also supported.
 */
class SpecialField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[String, OwnerType] with TypedField[String]
  with MandatoryTypedField[String] {

  override def owner = rec
  //override def classOfPersistentField = classOf[String]
  override def defaultValue = ""
  override def setFromString(s: String) = setBox(Full(s))
  override def setFromAny(c: Any) = c match {
    case Full(v) => setBox(Full(v.toString))
    case None => setBox(None)
    case v => setBox(Full(v.toString))
  }
  override def setFromJValue(jValue: JValue) = setBox(Full(jValue.toString))
  override def asJValue = JString(get)
  override def asJs = Str(get)
  override def toForm = Full(scala.xml.Text(get))
}

/**
 * Test record: An employee belongs to a company.
 */
class Employee private () extends Record[Employee] with NodeRecord[Employee] {

  override def meta = Employee

  val name = new SpecialField(this)
  val companyId = new LongField(this)
  val email = new EmailField(this, 100)
  val salary = new DecimalField(this, MathContext.UNLIMITED, 2)
  val locale = new LocaleField(this)
  val timeZone = new TimeZoneField(this)
  val password = new PasswordField(this)
  val photo = new OptionalBinaryField(this)
  val admin = new BooleanField(this)
  val departmentNumber = new IntField(this)
  val role = new EnumNameField(this, EmployeeRole)

  lazy val company = Company.parentOf(this)
  lazy val rooms = new MultiStringField(this)

}
object Employee extends Employee with MetaRecord[Employee]

/**
 * Test record: One or more employees can have a room (one-to-many-relation).
 */
class Room private () extends Record[Room] with NodeRecord[Room] {
  override def meta = Room

  val name = new StringField(this, 50)

  lazy val employees = getReferences[Employee] map { _._1 }
}

object Room extends Room with MetaRecord[Room]

object CompanySchema extends Schema(
  classOf[Company]) {
  
  import Path._
  import JcrRecord._

  /**
   * Creates some test instances of companies and employees
   * and saves them in the database.
   */
  def createTestData {
    import TestData._

    /*
    allCompanies.foreach(companies.insert(_))
    allEmployees.foreach(employees.insert(_))
    allRooms.foreach(rooms.insert(_))
    */
    
    e1.rooms.set(List(r1.identifier, r2.identifier))
    e1.save()
    jcrSession.save()
  }
  
  def cleanup {
    Company.find(root / "first") foreach { _.remove() }
    Company.find(root / "second") foreach { _.remove() }
    Company.find(root / "third") foreach { _.remove() }
    Room.find(root / "rooms") foreach { _.remove() }
  }

  object TestData {

    val c1 = Company.createAt(root / "first").name("First Company USA").
      created(Calendar.getInstance()).
      country(Countries.USA).postCode("12345")

    val c2 = Company.createAt(root / "second").name("Second Company USA").
      created(Calendar.getInstance()).
      country(Countries.USA).postCode("54321")

    val c3 = Company.createAt(root / "third").name("Company or Employee").
      created(Calendar.getInstance()).
      country(Countries.Canada).postCode("1234")

    val allCompanies = List(c1, c2, c3)

    lazy val e1 = Employee.createAt(root / "first" / "peter_example").
      name("Peter Example").
      email("peter@example.com").salary(BigDecimal(345)).
      locale(java.util.Locale.GERMAN.toString()).
      timeZone("Europe/Berlin").password("exampletest").
      admin(false).departmentNumber(2).role(EmployeeRole.Programmer).
      photo(Array[Byte](0, 1, 2, 3, 4))

    lazy val e2 = Employee.createAt(root / "second" / "test_employee").
      name("Company or Employee").
      email("test@example.com").salary(BigDecimal("123.123")).
      locale(java.util.Locale.US.toString()).
      timeZone("America/Los_Angeles").password("test").
      admin(true).departmentNumber(1).role(EmployeeRole.Manager).
      photo(Array[Byte](1))

    lazy val e3 = Employee.createAt(root / "second" / "joe_shmo").
      name("Joe Shmo").
      email("joe@shmo.com").salary(BigDecimal("100000.00")).
      locale(java.util.Locale.US.toString()).
      timeZone("America/Los_Angeles").password("test").
      admin(false).departmentNumber(1).role(EmployeeRole.Programmer).
      photo(Array[Byte](1))

    lazy val allEmployees = List(e1, e2, e3)

    val rooms = Room.createAt(root / "rooms")
    val r1 = Room.createAt(rooms.jcrPath.get / "room1").name("Room 1")
    val r2 = Room.createAt(rooms.jcrPath.get / "room2").name("Room 2")
    val r3 = Room.createAt(rooms.jcrPath.get / "room3").name("Room 3")

    lazy val allRooms = List(r1, r2, r3)
    
  }

}

