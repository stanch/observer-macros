package main

import org.scalatest.FlatSpec

class MainSpec extends FlatSpec with Tweaking with Tweaks {
  it should "infer type" in {
    new Button ~> on("click", () ⇒ println("much doge wow"))
  }
}