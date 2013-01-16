import org.specs2.mutable._
import scala.util.parsing.json._

import cc.hypo.Golem.service._

class GistSpec extends Specification {
  "Gist" should {
    // val gist = Gist("f29da084393ffaa32d908d97267dbf4a9de5dc41")
    val gist = Gist("Replace this with your access token")

    def test(description: String, public: Boolean, files: Set[GistFile]) = {
      val result = gist.jsonStringForGist(description = description, public = public, files = files)
      val j = JSON.parseFull(result)
      j.isDefined must beTrue
      val m = j.get
      println(m)

      m.isInstanceOf[Map[Any, Any]] must beTrue

      m.asInstanceOf[Map[String, Any]]("description") must beEqualTo(description)
      m.asInstanceOf[Map[String, Any]]("public") must beEqualTo(public)

      val gfiles: Map[String, Any] = m.asInstanceOf[Map[String, Any]]("files").asInstanceOf[Map[String, Any]]
      println(gfiles)

      gfiles.size must beEqualTo(files.size)
      files.foreach(f => {
        gfiles(f.filename).asInstanceOf[Map[String, Any]]("content") must beEqualTo(f.content)
      })
    }

    "Generate correct json " in {
      test("test", true, Set(GistFile("hello.json", "[1,2,3]")))
      test("with quotes!\"", true, Set(GistFile("he\\\"llo.json", "[1,2,\"3]")))
    }

    // "Create gist" in {
    //   gist.createGist("Example gist with test", false, Set(GistFile("hello.md", "# This is cool!\nhahaha")))
    // }
  }
}