package tests

import bill._
import scala.concurrent.Future
import scala.meta.io.AbsolutePath
import scala.meta.internal.metals.Messages
import scala.meta.internal.metals.ServerCommands
import scala.meta.internal.metals.Directories
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.RecursivelyDelete

class BillLspSuite extends BaseLspSuite("bill") {

  def globalBsp: AbsolutePath = workspace.resolve("global-bsp")
  override def bspGlobalDirectories: List[AbsolutePath] =
    List(globalBsp.resolve("bsp"))
  def testRoundtripCompilation(): Future[Unit] = {
    for {
      _ <- server.initialize(
        """
          |/src/com/App.scala
          |object App {
          |  val x: Int = ""
          |}
        """.stripMargin
      )
      _ <- server.didOpen("src/com/App.scala")
      _ = assertNoDiff(
        client.workspaceDiagnostics,
        """
          |src/com/App.scala:2:16: error: type mismatch;
          | found   : String("")
          | required: Int
          |  val x: Int = ""
          |               ^^
        """.stripMargin
      )
      _ <- server.didSave("src/com/App.scala")(_ => "object App")
      _ = assertNoDiff(
        client.workspaceDiagnostics,
        ""
      )
    } yield ()
  }

  test("diagnostics") {
    cleanWorkspace()
    Bill.installWorkspace(workspace.toNIO)
    testRoundtripCompilation()
  }

  test("reconnect") {
    cleanWorkspace()
    Bill.installWorkspace(workspace.toNIO)
    for {
      _ <- server.initialize(
        """
          |/src/com/App.scala
          |object App {
          |  val x: Int = ""
          |}
          |/shutdown-trace
          |true
        """.stripMargin
      )
      _ <- server.executeCommand(ServerCommands.ConnectBuildServer.id)
      _ <- server.executeCommand(ServerCommands.ConnectBuildServer.id)
      _ = {
        val logs = workspace
          .resolve(Directories.log)
          .readText
          .linesIterator
          .filter(_.startsWith("trace:"))
          .mkString("\n")
        assertNoDiff(
          logs,
          // Assert that "Connect to build server" waits for the shutdown
          // response from the build server before sending "initialize".
          """|trace: initialize
             |trace: shutdown
             |trace: initialize
             |trace: shutdown
             |trace: initialize
             |""".stripMargin
        )
      }
    } yield ()
  }

  test("global") {
    RecursivelyDelete(globalBsp)
    cleanWorkspace()
    Bill.installGlobal(globalBsp.toNIO)
    testRoundtripCompilation()
  }

  def testSelectServerDialogue(): Future[Unit] = {
    for {
      _ <- server.initialize(
        """
          |/src/App.scala
          |object App {}
        """.stripMargin
      )
      _ = assertNoDiff(
        client.workspaceMessageRequests,
        List(
          Messages.SelectBspServer.message,
          Messages.CheckDoctor.allProjectsMisconfigured
        ).mkString("\n")
      )
    } yield ()
  }

  test("conflict") {
    cleanWorkspace()
    Bill.installWorkspace(workspace.toNIO, "Bill")
    Bill.installWorkspace(workspace.toNIO, "Bob")
    testSelectServerDialogue()
  }

  test("mix") {
    cleanWorkspace()
    RecursivelyDelete(globalBsp)
    Bill.installWorkspace(workspace.toNIO, "Bill")
    Bill.installGlobal(globalBsp.toNIO, "Bob")
    testSelectServerDialogue()
  }
}
