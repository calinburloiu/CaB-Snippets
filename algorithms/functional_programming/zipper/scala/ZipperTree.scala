package org.calanbur.zipper.burloiu

sealed trait ZipperTree[+A]
case object NilNode extends ZipperTree[Nothing]
case class Node[+A](
    value: A,
    lefts: List[ZipperTree[A]],
    focus: ZipperTree[A],
    rights: List[ZipperTree[A]])
  extends ZipperTree[A] {
  
  override val toString: String =
      value + "(" + lefts.mkString("[", ",", "]") + "," + focus +
      "," + rights.mkString("[", ",", "]") + ")"
}

sealed trait Path[+A]
case object Top extends Path[Nothing]
case class PathNode[+A](
    lefts: List[ZipperTree[A]],
    up: Path[A],
    rights: List[ZipperTree[A]],
    parentValue: A)
  extends Path[A] {
  
  override val toString: String =
      "PathNode(" + lefts.mkString("[", ",", "]") + "," + up +
      "," + rights.mkString("[", ",", "]") + "," + parentValue + ")"
}

case class TreeLoc[+A](tree: ZipperTree[A], path: Path[A]) {
  
  override val toString = "" + tree
  
  def lefts: List[ZipperTree[A]] = path match { 
    case Top => Nil
    case p: PathNode[_] => p.lefts
  }
  
  def rights: List[ZipperTree[A]] = path match {
    case Top => Nil
    case p: PathNode[_] => p.rights
  }
  
  // NAVIGATION
  // ==========
  
  def left: Option[TreeLoc[A]] = path match {
    case Top => None
    case PathNode(Nil, _, _, _) => None
    case PathNode(l :: ls, u, rs, pv) =>
      Some(TreeLoc(l, PathNode(ls, u, tree :: rs, pv)))
  }
  
  def right: Option[TreeLoc[A]] = path match {
    case Top => None
    case PathNode(_, _, Nil, _) => None
    case PathNode(ls, u, r :: rs, pv) =>
      Some(TreeLoc(r, PathNode(tree :: ls, u, rs, pv)))
  }
  
  def up: Option[TreeLoc[A]] = path match {
    case Top => None
    case PathNode(ls, u, rs, pv) =>
      Some(TreeLoc(Node(pv, ls, tree, rs), u))
  }
  
  def down: Option[TreeLoc[A]] = tree match {
    case NilNode => None
    case Node(v, ls, f, rs) =>
      Some(TreeLoc(f, PathNode(ls, path, rs, v)))
  }
  
  // CHANGES, INSERTIONS AND DELETIONS
  // =================================
  // TODO Do we need node or tree?
  
  def change[B >: A](newTree: ZipperTree[B]): TreeLoc[B] = TreeLoc(newTree, path)
  
  def insertLeft[B >: A](newLeft: ZipperTree[B]): TreeLoc[B] = path match {
    case Top => throw new IllegalArgumentException("insert to left of top")
    case PathNode(ls, u, rs, pv) =>
      TreeLoc(tree, PathNode(newLeft :: ls, u, rs, pv))
  }
  
  def insertRight[B >: A](newRight: ZipperTree[B]): TreeLoc[B] = path match {
    case Top => throw new IllegalArgumentException("insert to right of top")
    case PathNode(ls, u, rs, pv) =>
      TreeLoc(tree, PathNode(ls, u, newRight :: rs, pv))
  }
  
  def insertDown[B >: A](newDown: ZipperTree[B]): TreeLoc[B] = tree match {
    case NilNode => throw new IllegalArgumentException("down of item")
    case Node(v, ls, f, rs) => TreeLoc(newDown, PathNode(ls, path, f :: rs, v))
  }
  
  def delete = path match {
    case Top => change(NilNode)
    
    // Delete focus and go to right if possible.
    case PathNode(ls, u, r :: rs, pv) => TreeLoc(r, PathNode(ls, u, rs, pv))
    
    // If nothing to right, delete focus and go to left.
    case PathNode(l :: ls, u, Nil, pv) => TreeLoc(l, PathNode(ls, u, Nil, pv))
    
    // If focus has no siblings replace with NilNode.
    case PathNode(Nil, u, Nil, pv) => change(NilNode)
  }
}

object ZipperTreeApp extends App {
  // Parse tree of the arithmetic expression: a*b*c + d*e*f
  val t1 = Node("*",
      Nil, 
      Node("a", Nil, NilNode, Nil),
      List(Node("b", Nil, NilNode, Nil), Node("c", Nil, NilNode, Nil)));
  val t2 = Node("*",
      Nil, 
      Node("d", Nil, NilNode, Nil),
      List(Node("e", Nil, NilNode, Nil), Node("f", Nil, NilNode, Nil)));
  val sum = Node("+", Nil, t1, List(t2))
  
  val sumLoc = TreeLoc(sum, Top)
  
  println(sum)
}