package main

class Main extends App with Tweaking {
  // explicit usage
  val setEmptyClick = tweak [Button] doing (_.setOnClick(()))
  new Button ~> setEmptyClick

  // inferred usage
  new Button ~> (tweak doing { _.setOnClick(()) })
  new Button ~> test

  // inferred as Widget
  def id(i: Int) = tweak doing (_.setId(i))
}