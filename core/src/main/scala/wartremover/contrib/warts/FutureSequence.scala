package org.wartremover
package contrib.warts

import scala.concurrent.Future

object FutureSequence extends WartTraverser {

  val message =
    """Future.sequence is considered a code smell.
      |Please make sure the execution context is setup correctly if you are using a large number of futures.""".stripMargin

  def apply(u: WartUniverse): u.Traverser = {
    import u.universe._

    val futureSymbol = typeOf[Future.type]
    val sequenceName = TermName("sequence")
    val sequenceSymbol = futureSymbol.member(sequenceName)

    require(futureSymbol != NoSymbol)
    require(sequenceSymbol != NoSymbol)

    new Traverser {
      override def traverse(tree: Tree): Unit = {
        tree match {
          // Ignore trees marked by SuppressWarnings
          case t if hasWartAnnotation(u)(t) =>
          case Apply(Apply(method, _), _) if method.symbol == sequenceSymbol =>
            warning(u)(tree.pos, message)
            super.traverse(tree)
          case _ =>
            super.traverse(tree)
        }
      }
    }
  }

}
