/**
  * Created by xu on 09/06/2017.
  */
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def apply[A](root: Tree[A], node: Tree[A]): Tree[A] = {
    Branch(root, node)
  }

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l,r) => maximum(l) max maximum(r)
  }

  def findMaxValue[Int](tree: Tree[Int], max_v: Int)(f: (Int, Int) => Int): Int = tree match {
    case Leaf(a) => f(a, max_v)
    case Branch(l, r) => f(findMaxValue(l, max_v)(f), findMaxValue(r, max_v)(f))
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeWithFold[A](tree: Tree[A]): Int = fold(tree)(a => 1)(1 + _ + _)

  def maxWithFold(tree: Tree[Int]): Int = fold(tree)(a => a)(_ max _)

  def depthWithFold[A](tree: Tree[A]): Int = fold(tree)(a => 0)((l, r) => 1 + (l max r))

  def mapWithFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))

  def begin(): Unit = {
    val tree1: Tree[Int] = Tree(Leaf(1), Branch(Leaf(2), Leaf(3)))
    val tree2: Tree[Int] = Branch(Branch(Leaf(5), Leaf(6)), Branch(Branch(Leaf(1), Leaf(4)), Branch(Leaf(2), Branch(Leaf(3), Leaf(7)))))
    //println(Tree.size(tree1))
    println("Max: " + Tree.findMaxValue(tree1, 0)((x:Int, y:Int) => if (x > y) x else y))
    println("Depth: " + Tree.depth(tree2))
  }
}