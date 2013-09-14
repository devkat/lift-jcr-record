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
import net.liftweb.record.TypedField

trait NodeRecord[T <: Record[T] with NodeRecord[T]] extends Record[T] with Mapper[T] {
  self: T =>

  import JcrRecord._
  import Extensions._
  
  protected def jcrPath(node:Node) = Path.parse(node.getPath)

  def jcrPath: Option[Path] = jcrNode map jcrPath _

  private[jcrrecord] var jcrNode: Option[Node] = None
  
  def identifier = withNode { _.getIdentifier }
  
  def createAt(path: Path): T = {
    val r = meta.createRecord
    val node = jcrSession.getRootNode.addNode(path.names mkString "/")
    r.jcrNode = Some(node)
    r
  }

  protected def findNodes(path: Path, parent: Node = jcrSession.getRootNode): Seq[Node] = {
    path.names match {
      case Nil => Nil
      case step :: Nil => parent.getNodes(step).toSeq
      case head :: tail => {
        val nodes = parent.getNodes(head)
        nodes.flatMap(n => findNodes(Path(tail, false), n)).toSeq
      }
    }
  }
  
  def find(path: Path): Seq[T] = {
    findNodes(path).map(fromNode _).toSeq
  }

  def fromNode(n:Node): T =
    { val r = meta.createRecord; r.load(n); r }
  
  def withNode[T](f: Node => T): T = jcrNode match {
    case Some(node) => f(node)
    case None => throw new RuntimeException("Record not bound to node.")
  }

  def remove() {
    withNode { _.remove() }
  }
  
  def parentOf(child:NodeRecord[_]): Option[T] = child withNode { n =>
    val parent = n.getParent
    parent.getPath() match {
      case "" => None
      case p => Some(fromNode(parent))
    }
  }
  
  def getReferences[R <: NodeRecord[R]]: Iterator[(T, TypedField[_])] = withNode { node =>
    node.getReferences map { property =>
      val refRecord = fromNode(property.getParent)
      val field = refRecord.fieldByName(property.getName)
      (refRecord, field.asInstanceOf[TypedField[_]])
    }
  }
  
}

