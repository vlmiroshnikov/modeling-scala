package pretty

import cats.*
import cats.syntax.all.*
import models.*

case class Node[A](label: A, siblings: List[Node[A]])

object Tree:
  def leaf[A](a: A): Node[A]                = Node(a, Nil)
  def node[A](a: A, sib: Node[A]*): Node[A] = Node(a, sib.toList)

given Traverse[Node] = new Traverse[Node] {

  override def foldLeft[A, B](fa: Node[A], b: B)(f: (B, A) => B): B =
    val newB = fa
      .siblings
      .foldLeft(b)((bb, n) => foldLeft(n, bb)(f))
    f(newB, fa.label)

  override def foldRight[A, B](fa: Node[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    val newB = fa
      .siblings
      .foldRight(lb)((n, bb) => foldRight(n, bb)(f))
    f(fa.label, newB)

  override def traverse[G[_]: Applicative, A, B](fa: Node[A])(f: A => G[B]): G[Node[B]] =
    (f(fa.label), fa.siblings.traverse(z => traverse(z)(f))).mapN(Node(_, _))
}

def draw(root: Node[String]): List[String] =
  def drawSubTrees(s: List[Node[String]]): List[String] =
    s match
      case Nil      => Nil
      case t :: Nil => "|" :: shift("`- ", "   ", draw(t))
      case t :: ts  => "|" :: shift("+- ", "|  ", draw(t)) ++ drawSubTrees(ts)

  def shift(first: String, other: String, s: List[String]): List[String] =
    s.zip(LazyList(first) ++ LazyList.continually(other)).map((a, b) => b.concat(a))

  root.label :: drawSubTrees(root.siblings)

/*
 /** A 2D String representation of this Tree, separated into lines. */
  def draw[B >: A](implicit sh: Show[B]): Stream[String] = {
    implicit val showa: Show[A] = sh contramap (x => x)
    def drawSubTrees(s: Stream[Tree[A]]): Stream[String] = s match {
      case Stream.Empty => Stream.Empty
      case Stream(t) => "|" #:: shift("`- ", "   ", t.draw)
      case t #:: ts => "|" #:: shift("+- ", "|  ", t.draw) append drawSubTrees(ts)
    }
    def shift(first: String, other: String, s: Stream[String]): Stream[String] =
      s.ʐ <*> ((first #:: other.repeat[Stream]).ʐ ∘ ((_: String) + (_: String)).curried)

    rootLabel.shows #:: drawSubTrees(subForest)
  }
 */

def asTree[A: Show](pr: Project[A]): Node[String] =
  pr match
    case Project.Leaf(name, a)            => Tree.leaf(s"${name} ${a.show}")
    case Project.Group(name, g, projects) => Tree.node(s"${name} ${g.show}", projects.map(asTree)*)
