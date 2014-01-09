package main

import scala.language.experimental.macros
import scala.reflect.macros.{BlackboxContext, WhiteboxContext}
import scala.annotation.implicitNotFound

/* Simple widget mocks */

class Widget {
  def setId(id: Int): Unit = ()
}
class Button extends Widget {
  def setOnClick(f: ⇒ Unit): Unit = ()
}
class Label extends Widget {
  def setText(text: String): Unit = ()
}

/** A tweak is something that mutates a widget */
case class Tweak[-W <: Widget](f: W ⇒ Unit) {
  def apply(w: W) = f(w)
}

@implicitNotFound("Don't know how to tweak ${W} with ${T}")
/** A typeclass for 'tweakable' relation */
trait TweakableWith[W, T] {
  def tweakWith(w: W, t: T): Unit
}

@implicitNotFound("Could not infer the type of the widget being tweaked. Please provide it explicitly.")
/** Tweaking context tracks the type of the widget(s) being tweaked */
trait WidgetType {
  type W <: Widget
}

/** Tweaking operations */
trait Tweaking {
  implicit class TweakingOps[W](w: W) {
    /** Tweak something with something */
    def ~>[T](t: T)(implicit tweakableWith: W TweakableWith T): W = { tweakableWith.tweakWith(w, t); w }
  }

  /** Widgets are tweakable with Tweaks (kinda obvious) */
  implicit def tweakableWithTweak[W <: Widget, T <: Tweak[W]] =
    new (W TweakableWith T) {
      def tweakWith(w: W, t: T) = t(w)
    }

  class TweakMaker[W <: Widget] {
    def doing(f: W ⇒ Unit) = Tweak(f)
  }

  /** Use this when you want to infer the type of the widget from the context */
  def tweak(implicit wtp: WidgetType) = new TweakMaker[wtp.W]

  /** Use this to provide widget type explicitly */
  def tweak[W <: Widget] = new TweakMaker[W]

  /** A macro that uses WidgetType) */
  def test(implicit wtp: WidgetType) = macro TweakingMacros.testImpl

  /** Infer widget type */
  implicit def inferWidgetType: WidgetType = macro TweakingMacros.inferWidgetType
}

object TweakingMacros {
  /** THE HACK! */
  def fromImmediateParentTree[A](c: BlackboxContext)(parent: PartialFunction[c.Tree, A]) = {
    import c.universe._

    // a parent contains the current macro application
    def isParent(x: Tree) = parent.isDefinedAt(x) & x.find(_.pos == c.macroApplication.pos).isDefined

    // an immediate parent is a parent and contains no other parents
    c.enclosingImpl.find { x ⇒
      isParent(x) && x.children.forall(_.find(isParent).isEmpty)
    } map { x ⇒
      parent(x)
    }
  }

  def inferWidgetType(c: WhiteboxContext) = {
    import c.universe._
    val tilde: PartialFunction[Tree, Type] = { case q"$x ~> $y" ⇒ c.typeCheck(x).tpe }
    val tp = fromImmediateParentTree(c)(tilde)
    writeWidgetType(c)(tp.getOrElse(typeOf[Widget]))
  }

  /** Create an instance of TweakingContext */
  def writeWidgetType(c: BlackboxContext)(widget: c.Type) = {
    import c.universe._
    q"new WidgetType { type W = $widget }"
  }

  /** Get W from TweakingContext */
  def readWidgetType(c: BlackboxContext)(wtp: c.Tree) = {
    import c.universe._
    val Block(List(ClassDef(_, _, _, Template(_, _, body))), _) = wtp
    val List(_, TypeDef(_, TypeName("W"), _, widget)) = body
    widget
  }

  def testImpl(c: WhiteboxContext)(wtp: c.Expr[WidgetType]) = {
    import c.universe._
    val tp = readWidgetType(c)(wtp.tree)
    q"Tweak[$tp](x ⇒ ())"
  }
}