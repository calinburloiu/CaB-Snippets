package org.calanbur.zipper.burloiu

sealed trait Tree[+A]
case class NilNode() extends Tree[Nothing]
case class Node[+A](
    value: A,
    lefts: List[Node[A]],
    children: List[Node[A]],
    rights: List[Node[A]],
    parent: Tree[A])
  extends Tree[A] {
  
  // NAVIGATION
  // ==========
  
  def left: Option[Node[A]] = lefts match {
    case Nil => None
    case l :: ls => Some(Node(l.value, ls, l.children, this :: rights, parent))
  }
  
  def right: Option[Node[A]] = rights match {
    case Nil => None
    case r :: rs => Some(Node(r.value, this :: lefts, r.children, rs, parent))
  }
  
  def up: Option[Node[A]] = parent match {
    case NilNode() => None
//    case Node(v, ls, cs, rs, p) => Node(p.v, )
    case p: Node[A] => Some(p)
  }
}

object ZipperTreeTest extends App {
  val t = Node("*", Nil, List(Node("a", Nil, Nil, List(Node("b")))))
}