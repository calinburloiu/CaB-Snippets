/**
 * Adapted from "Functional Pearl: The Zipper", Gerard Huet
 * INRIA Rocquencourt, France
 * J. Functional Programming 7 (5): 549–554, September 1997.
 * Copyright 1997 Cambridge University Press
 */
package org.calanbur.zipper.tree.huet

sealed trait Tree
case class Item(value: String) extends Tree
case class Section(forest: List[Tree]) extends Tree {
  override val toString = "Section" + forest.mkString("[", ",", "]")
}

sealed trait Path
case class Top() extends Path
case class Node(l: List[Tree], p: Path, r: List[Tree]) extends Path {
  override val toString = "Node(L" + l.mkString("[", ",", "]") + "," + p +
      ",R" + r.mkString("[", ",", "]") + ")";
}

sealed trait Location
case class Loc(tree: Tree, path: Path) extends Location {
  
  // NAVIGATION
  // ==========
  
  def left = path match {
    case Top() => throw new IllegalArgumentException("left of top")
    case Node(Nil, _, _) => throw new IllegalArgumentException("left of first")
    case Node(l :: left, up, right) => Loc(l, Node(left, up, tree :: right))
  }
  
  def right = path match {
    case Node(left, up, r :: right) => Loc(r, Node(tree :: left, up, right))
    case Node(_, _, Nil) => throw new IllegalArgumentException("right of last")
    case Top() => throw new IllegalArgumentException("right of top")
  }
  
  // More expensive than other operations: it depends on the "juniority" of
  // left (left.size).
  def up = path match {
    case Node(left, up, right) =>
      Loc(Section(left.reverse ++ (tree :: right)), up)
    case Top() => throw new IllegalArgumentException("up of top")
  }
  
  def down = tree match {
    case Section(firstTree :: forest) =>
      Loc(firstTree, Node(Nil, path, forest))
    case Item(_) => throw new IllegalArgumentException("down of item")
    case _ => throw new IllegalArgumentException("down of empty")
  }
  
  // CHANGES, INSERTIONS AND DELETIONS
  // =================================
  
  def change(newTree: Tree) = Loc(newTree, path)
  
  def insertLeft(newLeft: Tree) = path match {
    case Top() => throw new IllegalArgumentException("insert to left of top")
    case Node(left, up, right) => Loc(tree, Node(newLeft :: left, up, right))
  }
  
  def insertRight(newRight: Tree) = path match {
    case Top() => throw new IllegalArgumentException("insert to right of top")
    case Node(left, up, right) => Loc(tree, Node(left, up, newRight :: right))
  }
  
  def insertDown(newDown: Tree) = tree match {
    case Item(_) => throw new IllegalArgumentException("down of item")
    case Section(sons) => Loc(newDown, Node(Nil, path, sons))
  }
  
  def delete = path match {
    case Top() => throw new IllegalArgumentException("delete of top")
    case Node(left, up, r :: right) => Loc(r, Node(left, up, right))
    case Node(l :: left, up, Nil) => Loc(l, Node(left, up, Nil))
    case Node(Nil, up, Nil) => Loc(Section(Nil), up)
  }
}

object ZipperTree extends App {
  // Parse tree of the arithmetic expression: a*b + c*d
  val parseTree = 
    Section(List(
      Section(List(
        Item("a"),
        Item("*"),
        Item("b"))),
      Item("+"),
      Section(List(
        Item("c"),
        Item("*"),
        Item("d")))))
  
  // The location of the second multiplication sign in the parse tree.
  val secMulLoc =
    Loc(Item("*"),
      Node(
        List(Item("c")),
        Node(
          List(Item("+"), Section(List(Item("a"), Item("*"), Item("b")))),
          Top(),
          Nil),
        List(Item("d"))));
  
  println(secMulLoc)
}