package main

import scala.language.experimental.macros
import scala.reflect.macros.Context

class Widget
class Button extends Widget
class Label extends Widget

case class Tweak[-W <: Widget](f: W ⇒ Unit) {
  def apply(w: W) = f(w)
}

trait TweakableWith[W, T] {
  def tweakWith(w: W, t: T): Unit
}

trait Tweaking {
  implicit class Ops[W](w: W) {
    def ~>[T](t: T)(implicit tweakableWith: W TweakableWith T): W = { tweakableWith.tweakWith(w, t); w }
  }

  implicit def tweakableWithTweak[W <: Widget, V >: W <: Widget] =
    new (W TweakableWith Tweak[V]) {
      def tweakWith(w: W, t: Tweak[V]) = t(w)
    }

}

trait Tweaks {
  def text(t: String) = Tweak[Widget](x ⇒ ())
  def on[W <: Widget](event: String, action: () ⇒ Unit): Tweak[W] = macro TweakMacros.onImpl[W]
}

object TweakMacros {
  def onImpl[W <: Widget : c.WeakTypeTag](c: Context)(event: c.Expr[String], action: c.Expr[() ⇒ Unit]) = {
    import c.universe._
    c.info(c.enclosingPosition, s"type: ${weakTypeOf[W]}", force = true)
    c.Expr[Tweak[W]](q"Tweak[${weakTypeOf[W]}](x ⇒ ())")
  }
}