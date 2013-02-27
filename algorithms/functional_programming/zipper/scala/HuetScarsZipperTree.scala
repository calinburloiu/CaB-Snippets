/**
 * Adapted from "Functional Pearl: The Zipper", Gerard Huet
 * INRIA Rocquencourt, France
 * J. Functional Programming 7 (5): 549–554, September 1997.
 * Copyright 1997 Cambridge University Press
 */
package org.calanbur.zipper.tree.huet.scars

sealed trait Tree
case class Item(value: String) extends Tree
case class Node(lefts: List[Tree], tree: Tree, rights: List[Tree]) extends Tree {
  
  override val toString = "Node(L" + lefts.mkString("[", ",", "]") + "," + tree +
      ",R" + rights.mkString("[", ",", "]") + ")";
}

sealed trait Path
case class Top() extends Path
case class PathNode(lefts: List[Tree], up: Path, rights: List[Tree]) extends Path {
  
  override val toString = "Node(L" + lefts.mkString("[", ",", "]") + "," + up +
      ",R" + rights.mkString("[", ",", "]") + ")";
}

sealed trait Location
case class Loc(tree: Tree, path: Path) extends Location {
  
  // NAVIGATION
  // ==========
  
  def left = path match {
    case Top() => throw new IllegalArgumentException("left of top")
    case PathNode(Nil, _, _) => throw new IllegalArgumentException("left of first")
    case PathNode(l :: ls, u, rs) => Loc(l, PathNode(ls, u, tree :: rs))
  }
  
  def right = path match {
    case PathNode(ls, u, r :: rs) => Loc(r, PathNode(tree :: ls, u, rs))
    case PathNode(_, _, Nil) => throw new IllegalArgumentException("right of last")
    case Top() => throw new IllegalArgumentException("right of top")
  }
  
  def up = path match {
    case PathNode(ls, u, rs) =>
      Loc(Node(ls, tree, rs), u)
    case Top() => throw new IllegalArgumentException("up of top")
  }
  
  def down = tree match {
    case Node(ls, t, rs) =>
      Loc(t, PathNode(ls, path, rs))
    case Item(_) => throw new IllegalArgumentException("down of item")
    case _ => throw new IllegalArgumentException("down of empty")
  }
  
  // CHANGES, INSERTIONS AND DELETIONS
  // =================================
  
  def change(newTree: Tree) = Loc(newTree, path)
  
  def insertLeft(newLeft: Tree) = path match {
    case Top() => throw new IllegalArgumentException("insert to left of top")
    case PathNode(ls, u, rs) => Loc(tree, PathNode(newLeft :: ls, u, rs))
  }
  
  def insertRight(newRight: Tree) = path match {
    case Top() => throw new IllegalArgumentException("insert to right of top")
    case PathNode(ls, u, rs) => Loc(tree, PathNode(ls, u, newRight :: rs))
  }
  
//  def insertDown(newDown: Tree) = tree match {
//    case Item(_) => throw new IllegalArgumentException("down of item")
//    case Section(sons) => Loc(newDown, Node(Nil, path, sons))
//  }
  
//  def delete = path match {
//    case Top() => throw new IllegalArgumentException("delete of top")
//    case Node(ls, u, r :: rs) => Loc(r, Node(ls, u, rs))
//    case Node(l :: ls, u, Nil) => Loc(l, Node(ls, u, Nil))
//    case Node(Nil, u, Nil) => Loc(Section(Nil), u)
//  }
}

object ZipperTree extends App {
  // Parse tree of the arithmetic expression: a*b + c*d
//  val parseTree = 
//    Section(List(
//      Section(List(
//        Item("a"),
//        Item("*"),
//        Item("b"))),
//      Item("+"),
//      Section(List(
//        Item("c"),
//        Item("*"),
//        Item("d")))))
  
  // The location of the second multiplication sign in the parse tree.
//  val secMulLoc =
//    Loc(Item("*"),
//      Node(
//        List(Item("c")),
//        Node(
//          List(Item("+"), Section(List(Item("a"), Item("*"), Item("b")))),
//          Top(),
//          Nil),
//        List(Item("d"))));
//  
//  println(secMulLoc)
}