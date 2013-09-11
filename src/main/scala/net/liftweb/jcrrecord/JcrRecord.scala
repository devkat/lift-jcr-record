package net.liftweb.jcrrecord

import net.liftweb.common.Loggable
import net.liftweb.util.LoanWrapper
import javax.jcr.Repository
import javax.jcr.Session
import net.liftweb.util.DynoVar
import net.liftweb.common._
import javax.jcr.SimpleCredentials
import javax.jcr.Node

object JcrRecord extends Loggable {
  
  private object currentSession extends DynoVar[Session]
  
  def jcrSession = currentSession.get match {
    case Full(s) => s
    case _ => throw new RuntimeException("Not logged in to repository.")
  }
  
  def namespaceRegistry = jcrSession.getWorkspace.getNamespaceRegistry
  val recordNS = Namespace("ljr", "http://liftweb.net/record/jcr")
  
  def withRepo[T](repo: Repository)(f: => T): T = {
    val session = repo.login(new SimpleCredentials("admin", "admin".toCharArray))
    currentSession.set(session)
    
    if (!namespaceRegistry.getURIs.contains(recordNS.uri))
      namespaceRegistry.registerNamespace(recordNS.prefix, recordNS.uri)
    
    try {
      val user = session.getUserID
      val name = repo.getDescriptor(Repository.REP_NAME_DESC)
      logger.info("Logged in as %s to a %s repository.".format(user, name))
      val ret = f
      session.save()
      ret
    } finally { 
      session.logout()
      currentSession.set(null)
    }
  }

  def buildLoanWrapper(repo: Repository) = new LoanWrapper {
    override def apply[T](f: => T): T = withRepo(repo) {
      f
    }
  }

}