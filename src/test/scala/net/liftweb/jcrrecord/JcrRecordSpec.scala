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
      root.getNode("hello").remove()
      jcrSession.save()
      jcrSession.getRootNode.getNode("hello/world") must throwA(new PathNotFoundException)
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
    
    "Delete all objects" in {
      Company.find(root / "bec") foreach { _.remove() }
      jcrSession.save()
      Company.find(root / "bec") mustEqual Seq.empty
    }
  }

}