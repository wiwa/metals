package tests.hover

import tests.pc.BaseHoverSuite
import tests.BuildInfoVersions

class HoverNegativeSuite extends BaseHoverSuite {

  // @tgodzik Dotty seems to show the most enclosing symbol even if we hover on empty content
  override def excludedScalaVersions: Set[String] =
    Set(BuildInfoVersions.scala3)

  // Negative results should have an empty output.
  def checkNegative(
      name: String,
      original: String,
      compat: Map[String, String] = Map.empty
  ): Unit =
    check(name, original, expected = "", compat = compat)

  checkNegative(
    "block",
    """object a {
      |  val x = {
      |    @@
      |    List(y)
      |  }
      |}
      |""".stripMargin
  )

  checkNegative(
    "template",
    """object a {
      |    @@
      |  def foo = 2
      |}
      |""".stripMargin
  )

  checkNegative(
    "block2",
    """object a {
      |  def foo = {
      |    val x = 2
      |    @@
      |    x
      |  }
      |}
      |""".stripMargin
  )

  checkNegative(
    "val-keyword",
    """object a {
      |  v@@al x = 42
      |}
      |""".stripMargin
  )

  checkNegative(
    "val-equal",
    """object a {
      |  val x =@@ 42
      |}
      |""".stripMargin
  )

  checkNegative(
    "literal-int",
    """object a {
      |  val x = 4@@2
      |}
      |""".stripMargin
  )

  checkNegative(
    "literal-double",
    """object a {
      |  val x = 4@@2d
      |}
      |""".stripMargin
  )

  checkNegative(
    "literal-float",
    """object a {
      |  val x = 4@@2f
      |}
      |""".stripMargin
  )

  checkNegative(
    "literal-long",
    """object a {
      |  val x = 4@@2L
      |}
      |""".stripMargin
  )

  checkNegative(
    "literal-string",
    """object a {
      |  val x = "Hel@@lo"
      |}
      |""".stripMargin
  )

  checkNegative(
    "interpolator-part",
    """object a {
      |  val name = "John"
      |  s"Hel@@lo $name"
      |}
      |""".stripMargin
  )

}
