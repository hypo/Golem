import org.specs2.mutable._

import cc.hypo.Golem._

import org.jivesoftware.smack._
import org.jivesoftware.smack.packet._

class CommandSpec extends Specification {
  "RestoreCommand" should {
    val dispatcher = new RestoreCommandDispatcher(List())

    "Match 'restore [#]saleid' " in {
      dispatcher.process.isDefinedAt(Request(sender="yllan@hypo.cc", body="restore #123")) must beTrue
      dispatcher.process.isDefinedAt(Request(sender="yllan@hypo.cc", body="restore 12345")) must beTrue
      dispatcher.process.isDefinedAt(Request(sender="yllan@hypo.cc", body="restore ABCDEF")) must beFalse
      dispatcher.process.isDefinedAt(Request(sender="yllan@hypo.cc", body="store ABCDEF")) must beFalse
    }

    "Match 'restore [#]saleid to email@hypo.cc' " in {
      dispatcher.process.isDefinedAt(Request(sender="yllan@hypo.cc", body="restore #123 to yllan@hypo.cc")) must beTrue
    }    
  }
}