package cc.hypo.Golem.service

import org.apache.http.HttpEntity
import org.apache.http.HttpResponse
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.HttpPost
import org.apache.http.entity._
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.util.EntityUtils

import scala.util.parsing.json._
import scala.util._

case class GistFile(val filename: String, val content: String)

case class Gist(val token: String) {
  def jsonStringForGist(description: String, public: Boolean, files: Set[GistFile]): String = {
    val filesJson = files.map(gf => s""""${JSONFormat.quoteString(gf.filename)}":{"content":"${JSONFormat.quoteString(gf.content)}"}""").mkString("{", ",", "}")
    s"""
    {
      "description":"${JSONFormat.quoteString(description)}",
      "public":${public.toString},
      "files":${filesJson}
    }"""
  }

  def createGist(description: String, public: Boolean, files: Set[GistFile]): Try[String] = {
    val client = new DefaultHttpClient()
    try {
      val post = new HttpPost("https://api.github.com/gists")
      post.setHeader("Authorization", "token " + token)
      post.setEntity(new StringEntity(jsonStringForGist(description, public, files), ContentType.create("application/json", "UTF-8")))
      val response = client.execute(post)
      client.getConnectionManager.shutdown

      if (response.getStatusLine.getStatusCode == 201) {
        val body = EntityUtils.toString(response.getEntity)
        val m: Map[String, Any] = JSON.parseFull(body).get.asInstanceOf[Map[String, Any]]
        Success(m("html_url").asInstanceOf[String])
      } else {
        Failure(new Throwable("response status != 201"))
      }
    } catch {
      case e: Throwable => {
        client.getConnectionManager.shutdown
        Failure(e)
      }
    }
  }
}