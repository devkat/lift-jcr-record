package net.liftweb.jcrrecord

import net.liftweb.record.KeyedRecord
import java.util.UUID
import net.liftweb.record.Record
import net.liftweb.record.field._
import javax.jcr.Node
import Path._
import net.liftweb.record.Field
import javax.jcr.Property
import net.liftweb.record.MetaRecord

trait NodeRecord[T <: Record[T] with NodeRecord[T]] extends Record[T] with Mapper[T] {
  self: T =>

  import JcrRecord._
  import Extensions._

  def jcrPath: Option[Path] = jcrNode map { n => Path.parse(n.getPath) }

  private[jcrrecord] var jcrNode: Option[Node] = None
  
  def createAt(path: Path): T = {
    val r = meta.createRecord
    val node = jcrSession.getRootNode.addNode(path.names mkString "/")
    r.jcrNode = Some(node)
    r
  }

  protected def findNodes(path: Path): Iterator[Node] = {
    val root = jcrSession.getRootNode
    path.names match {
      case Nil => Iterator(root)
      case p => {
        val relPath = p mkString "/"
        if (root.hasNode(relPath)) root.getNodes(relPath) else Iterator.empty
      }
    }
  }

  def find(path: Path): Seq[T] =
    findNodes(path) map { node =>
      val r = meta.createRecord
      r.load(node)
      r
    } toSeq

  def withNode[T](f: Node => T): T = jcrNode match {
    case Some(node) => f(node)
    case None => throw new RuntimeException("Record not bound to node.")
  }

  def remove() {
    withNode { _.remove() }
  }
  
  private def dump(node: Node) {
    import Extensions._
    println("Path: " + node.getPath)

    for (property <- node.properties) {
      if (property.getDefinition.isMultiple)
        for (value <- property.getValues)
          println(" " + property.getPath + " = " + value)
      else
        println(" " + property.getPath + " = " +
          property.getString)
    }

    for (child <- node.childNodes)
      dump(child)
  }
}

