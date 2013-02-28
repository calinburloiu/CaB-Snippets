package org.calanbur.zipper.burloiu2

sealed trait ZipperList[+A]

object ZipperList {
  
  /**
   * Create a zipper list with one element.
   */
  def apply[A](elem: A): ZLNode[A] = ZLNode(Nil, elem, Nil)
  
  /**
   * Create a zipper list with more than one element.
   */
  def apply[A](elem0: A, elem1: A, elems: A*): ZLNode[A] =
    ZLNode(Nil, elem0, elem1 :: elems.toList)
}

case object ZLNil extends ZipperList[Nothing]

case class ZLNode[+A](
    lefts: List[A],
    focus: A,
    rights: List[A])
  extends ZipperList[A]


case class ZipperTree[+A](
    value: A,
    nephews: ZipperList[ZipperTree[A]]) {
  
  val (leftNephews: List[ZipperTree[A]],
      children: Option[ZipperTree[A]],
      rightNephews: List[ZipperTree[A]]) = nephews match {
    case ZLNil => (Nil, None, Nil)
    case node: ZLNode[_] => (node.lefts, Some(node.focus), node.rights)
  }

  override val toString: String =
      value + "(" + leftNephews.mkString("[", ",", "]") + "," + children +
      "," + rightNephews.mkString("[", ",", "]") + ")"
}

object ZipperTree {
  
  /**
   * Used to preserve compatibility with the previous version of ZipperTree.
   */
  def apply[A](value: A, lefts: List[ZipperTree[A]], focus: ZipperTree[A],
      rights: List[ZipperTree[A]]): ZipperTree[A] =
        ZipperTree(value, ZLNode(lefts, focus, rights))
  
  /**
   * Create a new node without children.
   */
  def apply[A](value: A): ZipperTree[A] = ZipperTree(value, ZLNil)
  
  /**
   * Create a new node with one child.
   */
  def apply[A](value: A, child: ZipperTree[A]): ZipperTree[A] =
    ZipperTree(value, ZipperList(child))
  
  /**
   * Create a new node with more than one child.
   */
  def apply[A](value: A, child0: ZipperTree[A], child1: ZipperTree[A],
      children: ZipperTree[A]*): ZipperTree[A] =
    ZipperTree(value, ZLNode(Nil, child0, child1 :: children.toList))
}


/**
 * Base class which abstract the path from a tree node to the root of the tree.
 * A path is represented like a linked list of PathNode-s which end in a PathTop
 * object. A PathNode holds the siblings (lefts, rights) of a set of ZipperTree-s
 * that go up to the top. These node are stored in the accompanying TreeLoc in
 * member tree.
 */
sealed trait Path[+A]
case object PathTop extends Path[Nothing]
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
  
  /**
   * Left siblings of current tree.
   */
  def lefts: List[ZipperTree[A]] = path match { 
    case PathTop => Nil
    case p: PathNode[_] => p.lefts
  }
  
  /**
   * Right siblings of current tree.
   */
  def rights: List[ZipperTree[A]] = path match {
    case PathTop => Nil
    case p: PathNode[_] => p.rights
  }
  
  // NAVIGATION
  // ==========
  
  /**
   * Go to the left sibling tree.
   */
  def left: Option[TreeLoc[A]] = path match {
    // Cannot go left.
    case PathTop => None
    case PathNode(Nil, _, _, _) => None
    
    case PathNode(l :: ls, u, rs, pv) =>
      Some(TreeLoc(l, PathNode(ls, u, tree :: rs, pv)))
  }
  
  /**
   * Go to the right sibling tree.
   */
  def right: Option[TreeLoc[A]] = path match {
    // Cannot go right.
    case PathTop => None
    case PathNode(_, _, Nil, _) => None
    
    case PathNode(ls, u, r :: rs, pv) =>
      Some(TreeLoc(r, PathNode(tree :: ls, u, rs, pv)))
  }
  
  /**
   * Go to the focused child tree.
   */
  def up: Option[TreeLoc[A]] = path match {
    // Cannot go up.
    case PathTop => None
    
    case PathNode(ls, u, rs, pv) =>
      Some(TreeLoc(ZipperTree(pv, ZLNode(ls, tree, rs)), u))
  }
  
  /**
   * Go to the focused parent tree.
   */
  def down: Option[TreeLoc[A]] = tree match {
    // Cannot go down.
    case ZipperTree(_, ZLNil) => None
    
    case ZipperTree(v, ZLNode(ls, f, rs)) =>
      Some(TreeLoc(f, PathNode(ls, path, rs, v)))
  }
  
  // CHANGES, INSERTIONS AND DELETIONS
  // =================================
  
  /**
   * Change the focused tree with another one.
   */
  def change[B >: A](newTree: ZipperTree[B]): TreeLoc[B] = TreeLoc(newTree, path)
  
  /**
   * Insert a node to the left of the focused one and focus it.
   */
  def insertLeft[B >: A](newLeft: ZipperTree[B]): TreeLoc[B] = path match {
    // Cannot insert to left of top.
    case PathTop => throw new IllegalArgumentException("insert to left of top")
    
    case PathNode(ls, u, rs, pv) =>
      TreeLoc(newLeft, PathNode(ls, u, tree :: rs, pv))
  }
  
  /**
   * Insert a node to the right of the focused one and focus it.
   */
  def insertRight[B >: A](newRight: ZipperTree[B]): TreeLoc[B] = path match {
    // Cannot insert to right of top.
    case PathTop => throw new IllegalArgumentException("insert to right of top")
    
    case PathNode(ls, u, rs, pv) =>
      TreeLoc(newRight, PathNode(tree :: ls, u, rs, pv))
  }
  
  /**
   * Insert a node to the left of the focused child node, or create the first
   * child of the focused node if it has none (its child is a NilNode).
   */
  def insertDown[B >: A](newDown: ZipperTree[B]): TreeLoc[B] = tree match {
    // Create the first child of a node.
    case ZipperTree(v, ZLNil) => TreeLoc(newDown, PathNode(Nil, path, Nil, v))
    
    // Insert a node to the left of the focused child.
    case ZipperTree(v, ZLNode(ls, f, rs)) => TreeLoc(newDown, PathNode(ls, path, f :: rs, v))
  }
  
  /**
   * Delete the focused node and move focus to the right if possible.
   * If there is no right sibling, go to left. If no siblings are available,
   * delete and go up.
   */
  def delete = path match {
    // Deleting the top node doesn't make sense.
    case PathTop => throw new IllegalArgumentException("delete of top")
    
    // Delete focus and go to right if possible.
    case PathNode(ls, u, r :: rs, pv) => TreeLoc(r, PathNode(ls, u, rs, pv))
    
    // If nothing to right, delete focus and go to left.
    case PathNode(l :: ls, u, Nil, pv) => TreeLoc(l, PathNode(ls, u, Nil, pv))
    
    // If focus has no siblings replace delete node and go to parent.
    case PathNode(Nil, u, Nil, pv) => TreeLoc(ZipperTree(pv), u)
  }
}

object TreeLoc {
  def apply[A](tree: ZipperTree[A]): TreeLoc[A] = TreeLoc(tree, PathTop)
}


object ZipperTreeApp extends App {
  // Parse tree of the arithmetic expression: +(*(a, b, c), *(d, e, f))
  val sum =
    ZipperTree("+",
      ZipperTree("*",
        ZipperTree("a"),
        ZipperTree("b"),
        ZipperTree("c")),
      ZipperTree("*",
        ZipperTree("d"),
        ZipperTree("e"),
        ZipperTree("f")));
  
  val sumLoc = TreeLoc(sum)
  var x = sumLoc
  x = x.down.getOrElse(x)
  x = x.right.getOrElse(x)
  
  println(x)
}