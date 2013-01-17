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

  val dispatchers = List(
    new RestoreCommandDispatcher(
      admins = config.stringListOpt("authorized-users") getOrElse List(), 
      consolePath = config.stringOpt("restore-dispatcher.rails-console-path").getOrElse("/bin/cat")
    ),
    new JsonCommandDispatcher(
      admins = config.stringListOpt("authorized-users") getOrElse List(), 
      consolePath = config.stringOpt("restore-dispatcher.rails-console-path").getOrElse("/bin/cat"),
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

  def requireExistSale(consolePath: String, saleID: String)(f: => String): String = {
    val checkSaleExpression = s"(HypoOrder.find_by_sale_id $saleID) != nil"
    if (!runCommandWithStdin(consolePath, checkSaleExpression).mkString.contains("=> true")) 
      s"找不到 #${saleID}"
    else 
      f
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

class JsonCommandDispatcher(val admins: List[String], val consolePath: String, val gistToken: String) extends AnyRef with CommandDispatcher {
  def reformat(json: String) = {
    val tab = "  "

    @tailrec 
    def reformatIter(newJson: String, original: String, previousChar: Char, inString: Boolean, indentLevel: Int): String = {
      if (original.length == 0) newJson
      else {
        val currentChar = original.head
        val (formatted, newInString, newIndentLevel) = currentChar match {
          case '{' | '[' if !inString ⇒ (currentChar + "\n" + tab * indentLevel + 1, inString, indentLevel + 1)
          case '}' | ']' if !inString ⇒ ("\n" + tab * (indentLevel - 1) + currentChar, inString, indentLevel - 1)
          case ',' if !inString ⇒ (",\n" + tab * indentLevel, inString, indentLevel)
          case ':' if !inString ⇒ (": ", inString, indentLevel)
          case ' ' | '\n' | '\t' if !inString ⇒ ("", inString, indentLevel)
          case '"' ⇒ (currentChar, if (previousChar == '\'') inString else !inString, indentLevel)
          case c ⇒ (currentChar, inString, indentLevel)
        }
        reformatIter(newJson + formatted, original.tail, currentChar, newInString, newIndentLevel)
      }
    }

    reformatIter("", json, ' ', false, 0)
  }

  def process = {
    case VerbWithSaleID("json", saleID, nil, req) ⇒ requireAdmin(req.sender, admins) {
      requireExistSale(consolePath, saleID) {
        val tempFilePath = s"/tmp/${saleID}-${scala.util.Random.alphanumeric.take(10)}.json"
        val getDataExpression = 
          s"""  o = HypoOrder.find_by_sale_id $saleID;
                File.open("$tempFilePath", "w") {|f| f.write(o.data); f.close }
          """
        runCommandWithStdin(consolePath, getDataExpression)
        val jsonContent = reformat(scala.io.Source.fromFile(new File(tempFilePath)).mkString)

        Gist(gistToken).createGist(s"JSON for $saleID at ${new java.util.Date}", false, Set(GistFile("${saleID}.json", jsonContent))) match {
          case Success(url) ⇒ url
          case Failure(e) ⇒ e.toString
        }
      }
    }
  }
}

class RestoreCommandDispatcher(val admins: List[String], val consolePath: String) extends AnyRef with CommandDispatcher {
  
  def restoreSaleToEditor(saleID: String, toAccount: Option[String] = None): String = requireExistSale(consolePath, saleID) {
    val restoreExpression = s"o = HypoOrder.find_by_sale_id $saleID;" +
                            "b = o.to_book;" +
                            toAccount.map(account ⇒ s"b.user = '$account';").getOrElse("") +
                            "b.save"
    val output = runCommandWithStdin(consolePath, restoreExpression)
    if (output.mkString.contains("=> true")) s"成功放回 #$saleID" + toAccount.map(account ⇒ s" to $account").getOrElse("")
    else "好像有錯誤喔：\n" + output.mkString("\n")  
  }

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
    case req: Request ⇒ s"什麼是 '${req.body}'？"
  }
}