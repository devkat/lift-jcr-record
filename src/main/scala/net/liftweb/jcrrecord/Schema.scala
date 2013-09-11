package net.liftweb.jcrrecord

import org.jcrom.Jcrom
import net.liftweb.record.MetaRecord
import net.liftweb.record.Record
import JcrRecord._
import javax.jcr.Node
import net.liftweb.record.Field
import net.liftweb.record.field._

abstract class Schema (val classes:Class[_ <: NodeRecord[_]]*) {
  
  import Extensions._

  def removeAll() {
    jcrSession.getRootNode.childNodes foreach { _.remove() }
  }
  
}