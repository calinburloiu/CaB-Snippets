package org.calanbur.zipper.burloiu

/**
 * Base class for case classes than describe a tree where nodes (Node) hold
 * values and leafs (NilNode) are unaccessible and only delimit the end of the
 * path.
 * 
 * To obtain the effect of a leaf node that holds data create a Node with a
 * single child of type NilNode.
 */
sealed trait ZipperTree[+A]

/**
 * A special tree node which marks its parent as a leaf node holding a value. It
 * should have no siblings.
 */
case object NilNode extends ZipperTree[Nothing]

/**
 * A tree node which holds a value.
 */
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

object Node {
  
  /**
   * Create a new node without children.
   */
  def apply[A](value: A): Node[A] = Node(value, Nil, NilNode, Nil)
  
  /**
   * Create a new node with one child.
   */
  def apply[A](value: A, child: Node[A]): Node[A] = Node(value, Nil, child, Nil)
  
  /**
   * Create a new node with more than one child.
   */
  def apply[A](value: A, child: Node[A], children: Node[A]*): Node[A] =
    Node(value, Nil, child, children.toList)
}


/**
 * Base class which abstract to path from a tree node to the root of the tree.
 * A path is represented like a linked list of PathNode-s which end in a Top
 * object. A PathNode holds the siblings (lefts, rights) of a set of Node-s
 * that go up to the top. These node are stored in the accompanying TreeLoc in
 * member tree.
 */
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
  
  /**
   * Left siblings of current tree.
   */
  def lefts: List[ZipperTree[A]] = path match { 
    case Top => Nil
    case p: PathNode[_] => p.lefts
  }
  
  /**
   * Right siblings of current tree.
   */
  def rights: List[ZipperTree[A]] = path match {
    case Top => Nil
    case p: PathNode[_] => p.rights
  }
  
  // NAVIGATION
  // ==========
  
  /**
   * Go to the left sibling tree.
   */
  def left: Option[TreeLoc[A]] = path match {
    // Cannot go left.
    case Top => None
    case PathNode(Nil, _, _, _) => None
    
    case PathNode(l :: ls, u, rs, pv) =>
      Some(TreeLoc(l, PathNode(ls, u, tree :: rs, pv)))
  }
  
  /**
   * Go to the right sibling tree.
   */
  def right: Option[TreeLoc[A]] = path match {
    // Cannot go right.
    case Top => None
    case PathNode(_, _, Nil, _) => None
    
    case PathNode(ls, u, r :: rs, pv) =>
      Some(TreeLoc(r, PathNode(tree :: ls, u, rs, pv)))
  }
  
  /**
   * Go to the focused child tree.
   */
  def up: Option[TreeLoc[A]] = path match {
    // Cannot go up.
    case Top => None
    
    case PathNode(ls, u, rs, pv) =>
      Some(TreeLoc(Node(pv, ls, tree, rs), u))
  }
  
  /**
   * Go to the focused parent tree.
   */
  def down: Option[TreeLoc[A]] = tree match {
    // Cannot go down.
    case NilNode => None
    case Node(_, _, NilNode, _) => None
    
    case Node(v, ls, f, rs) =>
      Some(TreeLoc(f, PathNode(ls, path, rs, v)))
  }
  
  // CHANGES, INSERTIONS AND DELETIONS
  // =================================
  
  /**
   * Change the focused tree with another one.
   */
  def change[B >: A](newTree: ZipperTree[B]): TreeLoc[B] = newTree match {
    case NilNode => throw new IllegalArgumentException("only insert Node objects")
    
    case Node(_, _, _, _) => TreeLoc(newTree, path)
  }
  
  /**
   * Insert a node to the left of the focused one and focus it.
   */
  def insertLeft[B >: A](newLeft: ZipperTree[B]): TreeLoc[B] = newLeft match { 
    case NilNode => throw new IllegalArgumentException("only insert Node objects")
    
    case Node(_, _, _, _) => path match {
      // Cannot insert to left of top.
      case Top => throw new IllegalArgumentException("insert to left of top")
      
      case PathNode(ls, u, rs, pv) =>
        TreeLoc(newLeft, PathNode(ls, u, tree :: rs, pv))
    }
  }
  
  /**
   * Insert a node to the right of the focused one and focus it.
   */
  def insertRight[B >: A](newRight: ZipperTree[B]): TreeLoc[B] = newRight match {
    case NilNode => throw new IllegalArgumentException("only insert Node objects")
    
    case Node(_, _, _, _) => path match {
      // Cannot insert to right of top.
      case Top => throw new IllegalArgumentException("insert to right of top")
      
      case PathNode(ls, u, rs, pv) =>
        TreeLoc(newRight, PathNode(tree :: ls, u, rs, pv))
    }
  }
  
  /**
   * Insert a node to the left of the focused child node, or create the first
   * child of the focused node if it has none (its child is a NilNode).
   */
  def insertDown[B >: A](newDown: ZipperTree[B]): TreeLoc[B] = newDown match {
    case NilNode => throw new IllegalArgumentException("only insert Node objects")
    
    case Node(_, _, _, _) => tree match {
      // This should not normally happen.
      case NilNode => throw new IllegalArgumentException("down of item")
      
      // Create the first child of a node.
      case Node(v, ls, NilNode, rs) => TreeLoc(newDown, PathNode(ls, path, rs, v))
      
      // Insert a node to the left of the focused child.
      case Node(v, ls, f, rs) => TreeLoc(newDown, PathNode(ls, path, f :: rs, v))
    }
  }
  
  /**
   * Delete the focused node and move focus to the right if possible.
   * If there is no right sibling, go to left. If no siblings are available,
   * delete and go up.
   */
  def delete = path match {
    // Deleting the top node doesn't make sense.
    case Top => throw new IllegalArgumentException("delete of top")
    
    // Delete focus and go to right if possible.
    case PathNode(ls, u, r :: rs, pv) => TreeLoc(r, PathNode(ls, u, rs, pv))
    
    // If nothing to right, delete focus and go to left.
    case PathNode(l :: ls, u, Nil, pv) => TreeLoc(l, PathNode(ls, u, Nil, pv))
    
    // If focus has no siblings replace delete node and go to parent.
    case PathNode(Nil, u, Nil, pv) => TreeLoc(Node(pv), u)
  }
}

object TreeLoc {
  def apply[A](tree: Node[A]): TreeLoc[A] = TreeLoc(tree, Top)
}


object ZipperTreeApp extends App {
  // Parse tree of the arithmetic expression: +(*(a, b, c), *(d, e, f))
  val sum =
    Node("+",
      Node("*",
        Node("a"),
        Node("b"),
        Node("c")),
      Node("*",
        Node("d"),
        Node("e"),
        Node("f")));
  
  val sumLoc = TreeLoc(sum)
  var x = sumLoc
  x = x.down.getOrElse(x)
  x = x.right.getOrElse(x)
  
  println(x)
}