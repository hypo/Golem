package cc.hypo.Golem

import org.jivesoftware.smack._
import org.jivesoftware.smack.packet._
import scala.sys.process._
import com.typesafe.config._
import scala.collection.JavaConverters._
import java.io._
import scala.util._
import scala.annotation._

import cc.hypo.Golem.service._

/* Add convenient method to Config */
object Implicits {
  import scala.language.implicitConversions

  class ConfigWrapper(val config: Config) {
    def stringOpt(path: String): Option[String] = {
      if (config.hasPath(path)) Some(config.getString(path))
      else None
    }

    def stringListOpt(path: String): Option[List[String]] = {
      if (config.hasPath(path)) Some(config.getStringList(path).asScala.toList)
      else None
    }
  }

  implicit def configToWrapper(config: Config): ConfigWrapper = new ConfigWrapper(config)
  implicit def wrapperToConfig(wrapper: ConfigWrapper): Config = wrapper.config

  class ChatExtensionWrapper(val chat: Chat) {
    def reply(msg: String) = {
      chat.sendMessage(msg)
      println("[SAY] " + msg)
    }
  }

  implicit def chatToExtensionWrapper(chat: Chat): ChatExtensionWrapper = new ChatExtensionWrapper(chat)
  implicit def extensionWrapperToChat(wrapper: ChatExtensionWrapper): Chat = wrapper.chat
}


case class Request(sender: String, body: String)


class SentenceListener(val config: Config) extends MessageListener {
  import Implicits._

  override def processMessage(chat: Chat, message: Message): Unit = {
    println("[RCV] " + message.getFrom + ": " + message.getBody)
    chat reply composedDispatcher(Request(sender = message.getFrom, body = message.getBody))
  }

  val editorDB = EditorDatabase(
    url = config.stringOpt("restore-dispatcher.editor-db-url").getOrElse("jdbc:mysql://localhost/defaultdb"),
    user = config.stringOpt("restore-dispatcher.editor-db-user").getOrElse("hypo"),
    password = config.stringOpt("restore-dispatcher.editor-db-password").getOrElse("pass"),
    socketFile = config.stringOpt("my-sql-socket").getOrElse("/var/mysqld.sock")
    )

  val dispatchers = List(
    new RestoreCommandDispatcher(
      admins = config.stringListOpt("authorized-users") getOrElse List(), 
      editorDB = editorDB
    ),
    new JsonCommandDispatcher(
      admins = config.stringListOpt("authorized-users") getOrElse List(), 
      editorDB = editorDB,
      gistToken = config.stringOpt("gist-token").getOrElse("NO TOKEN")
    ),
    new UnknowCommandDispatcher()
  )

  // Expand the dispathers (d1, d2, d3, …) to 
  //   d1.process orElse d2.process orElse d3.process ...
  val composedDispatcher = dispatchers.map(_.process).reduceLeft((f1, f2) => f1 orElse f2)
}

trait CommandDispatcher extends CommandLineProcessTool {
  def process: PartialFunction[Request, String]

  def requireAdmin(sender: String, admins: List[String])(f: => String): String = {
    if (admins.exists(admin ⇒ sender.startsWith(admin + "/"))) f
    else "你不是管理者，你壞壞。"
  }

  def requireExistSale(editorDB: EditorDatabase, saleID: String)(f: (HypoOrder) => String): String = {
    editorDB.orderForSaleID(saleID).map(f(_)).getOrElse(s"找不到 #${saleID}")
  }
}

trait CommandLineProcessTool {
  def runCommandWithStdin(command: String, stdin: String): Seq[String] = {
    println("Running: " + command)
    println("With: " + stdin)
    val output: Seq[String] = (Process(command) #< new ByteArrayInputStream(s"$stdin\n" getBytes "UTF-8")).lines_!
    println("Output: " + output.mkString("\n"))
    output
  }
}

object VerbWithSaleID {
  val VerbPattern = """(?i)(\w+)""".r
  /* the (?i)  makes the match case insensitive the complete set of options are:
  (?idmsux)
  i - case insensitive
  d - only unix lines are recognized as end of line
  m - enable multiline mode
  s - . matches any characters including line end
  u - Enables Unicode-aware case folding
  x - Permits whitespace and comments in pattern
  */
  val SaleIDPattern = """#?(\d+)""".r

  def unapply(req: Request) = req.body.split(" ").toList match {
    case VerbPattern(verb) :: SaleIDPattern(saleID) :: restArgs ⇒ Some((verb.toLowerCase, saleID, restArgs, req))
    case _ ⇒ None
  }
}

class JsonCommandDispatcher(val admins: List[String], val editorDB: EditorDatabase, val gistToken: String) extends AnyRef with CommandDispatcher {
  // For more readable but slower version, check: https://gist.github.com/raw/4603514/1eb0e8866b5f536ba7d4a7bb2d020d6e285b19e1/stringbuilder.scala
  def reformat(json: String) = {
    val tab = "  "
    var inString = false
    var indentLevel = 0
   
    var index = 0;
    val stringLength = json.length
    val formatted = new StringBuilder
   
    while (index < stringLength) {
      val c = json.charAt(index)
      if (!inString && (c == '{' || c == '[')) {
        indentLevel += 1
        formatted.append(c).append("\n" + (tab * indentLevel))
      } else if (!inString && (c == '}' || c == ']')) {
        indentLevel -= 1
        formatted.append("\n" + (tab * indentLevel)).append(c)
      } else if (!inString && c == ',') {
        formatted.append(",\n" + (tab * indentLevel))
      } else if (!inString && c == ':') {
        formatted.append(": ")
      } else if (!inString && (c == ' ' || c == '\n' || c == '\t')) {
        /* skip */
      } else if (c == '"') {
        formatted.append(c)
        if (index > 0 && json.charAt(index - 1) != '\'') inString = !inString
      } else {
        formatted.append(c)
      }
      index += 1
    }
    formatted.toString
  }


  def process = {
    case VerbWithSaleID("json", saleID, nil, req) ⇒ requireAdmin(req.sender, admins) {
      requireExistSale(editorDB, saleID) ((o: HypoOrder) => {
        val jsonContent = reformat(o.extractedData.getOrElse(""))
        Gist(gistToken).createGist(s"JSON for $saleID at ${new java.util.Date}", false, Set(GistFile(saleID + ".json", jsonContent))) match {
          case Success(url) ⇒ url
          case Failure(e) ⇒ e.toString
        }
      })
    }
  }
}

class RestoreCommandDispatcher(val admins: List[String], val editorDB: EditorDatabase) extends AnyRef with CommandDispatcher {
  
  def restoreSaleToEditor(saleID: String, toAccount: Option[String] = None): String = 
    requireExistSale(editorDB, saleID) ((o: HypoOrder) => {
      val b = o.toBook.copy(user = (toAccount orElse o.user))
      editorDB.insertBook(b)
      s"成功放回 #$saleID" + toAccount.map(account ⇒ s" to $account").getOrElse("")    
  })

  def process = {
    case VerbWithSaleID("restore", saleID, Nil, req) ⇒ requireAdmin(req.sender, admins) {
      restoreSaleToEditor(saleID)
    }
    case VerbWithSaleID("restore", saleID, List("to", email), req) ⇒ requireAdmin(req.sender, admins) {
      restoreSaleToEditor(saleID, Some(email))
    }
    case VerbWithSaleID("restore", saleID, List("to", email, mailtoLink), req) ⇒ requireAdmin(req.sender, admins) {
      restoreSaleToEditor(saleID, Some(email))
    }
  }
}

class UnknowCommandDispatcher extends AnyRef with CommandDispatcher {
  def process = {
    case req: Request ⇒ {
      s"什麼是 '${req.body}'？"
    }
  }
}