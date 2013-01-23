package cc.hypo.Golem.service

import dispatch._
import spray.json._
import DefaultJsonProtocol._ 

import scala.util._

case class GistFile(val filename: String, val content: String)

case class Gist(val token: String) {
  def jsonStringForGist(description: String, public: Boolean, files: Set[GistFile]): String = {
    val filesJson = files.map(gf ⇒ (gf.filename -> Map("content" -> gf.content).toJson)).toMap.toJson
    Map("description" -> description.toJson, "public" -> public.toJson, "files" -> filesJson).toJson.compactPrint
  }

  def createGist(description: String, public: Boolean, files: Set[GistFile]): Try[String] = {
    val github = url("https://api.github.com/gists").POST << jsonStringForGist(description, public, files) <:< 
                 Map("Authorization" -> s"token $token")

    val gist = Http(github OK as.String)
    val resp = gist().asJson.convertTo[Map[String, JsValue]]
    Success(resp("html_url").convertTo[String])
  }
}