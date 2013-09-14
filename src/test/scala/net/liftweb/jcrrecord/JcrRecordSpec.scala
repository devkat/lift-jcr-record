package net.liftweb.jcrrecord

import org.specs2.specification.AroundExample
import org.specs2.mutable.Specification
import net.liftweb.http.LiftSession
import net.liftweb.util.Helpers
import net.liftweb.common._
import org.specs2.execute.Result
import net.liftweb.http.S
import org.specs2.execute.AsResult
import org.apache.jackrabbit.core.TransientRepository
import JcrRecord._
import javax.jcr.PathNotFoundException
import Path._

class JcrRecordSpec extends Specification with AroundExample {
  
  import Extensions._
  import JcrHelpers._

  "JCR Record Specification".title
  sequential

  lazy val session = new LiftSession("", Helpers.randomString(20), Empty)
  protected def around[T: AsResult](t: => T): Result = {
    S.initIfUninitted(session) {
      JcrRecord.withRepo(new TransientRepository) {
        AsResult(t)
      }
    }
  }

  "JCR Record" should {
    "Create new nodes" in {
      val root = jcrSession.getRootNode
      
      // Store content 
      val hello = root.addNode("hello")
      val world = hello.addNode("world")
      world.setProperty("message", "Hello, World!")
      jcrSession.save()

      // Retrieve content 
      val node = root.getNode("hello/world")
      node.getPath() mustEqual "/hello/world"
      node.getProperty("message").getString mustEqual "Hello, World!"

      // Remove content 
      root.getNodes("hello") foreach {_.remove()}
      jcrSession.save()
      root.hasNode("hello/world") mustEqual false
    }
    
    "Create an object" in {
      val company = Company.createAt(root / "bec")
      company.name.set("BeCompany GmbH")
      company.save()
      company.name.get mustEqual "BeCompany GmbH"
    }
    
    "Retrieve an object" in {
      val company = Company.find(root / "bec").headOption.get
      company.name.get mustEqual "BeCompany GmbH"
    }
    
    "Delete objects" in {
      Company.find(root / "bec") foreach { _.remove() }
      jcrSession.save()
      Company.find(root / "bec") mustEqual Seq.empty
    }
    
    "Create test data" in {
      CompanySchema.createTestData
      1 mustEqual 1
    }
    
    "Manage references" in {
      val r1 = Room.find(root / "rooms" / "room1").head
      val r2 = Room.find(root / "rooms" / "room2").head
      val rooms = List(r1, r2)
      val employee = Employee.find(root / "first" / "peter_example").head
      employee.rooms.get mustEqual (rooms map (_.identifier))
    }
    
    "Clean up" in {
      CompanySchema.cleanup
      1 mustEqual 1
    }
  }

}