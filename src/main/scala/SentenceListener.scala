package cc.hypo.Golem

import org.jivesoftware.smack._
import org.jivesoftware.smack.packet._
import scala.sys.process._
import com.typesafe.config._
import scala.collection.JavaConverters._

class SentenceListener extends MessageListener {
  val dispatchers = List(
    new RestoreCommandDispatcher(),
    new UnknowCommandDispatcher()
  )
  val composedDispatcher = dispatchers.map(_.process).reduceLeft((f1, f2) => f1 orElse f2)

  override def processMessage(chat: Chat, message: Message): Unit = {
    println("[RCV] " + message.getFrom + ": " + message.getBody)
    composedDispatcher(chat, message)
  }	
}

trait CommandDispatcher {
  def process: PartialFunction[(Chat, Message), Option[CommandDispatcher]]

  class ChatExtensionWrapper(val chat: Chat) {
    def reply(msg: String) = {
      chat.sendMessage(msg)
      println("[SAY] " + msg)
    }
  }

  implicit def chatToExtensionWrapper(chat: Chat): ChatExtensionWrapper = new ChatExtensionWrapper(chat)
  implicit def extensionWrapperToChat(wrapper: ChatExtensionWrapper): Chat = wrapper.chat
}

object VerbWithSaleID {
  val VerbWithSaleIDPattern = """(?i)(\w+)\s*#?(\d+)""".r
  def unapply(m: Message) = m.getBody match {
    case VerbWithSaleIDPattern(verb, saleID) => Some((verb.toLowerCase, saleID, m.getFrom))
    case _ => None
  }
}

class RestoreCommandDispatcher extends AnyRef with CommandDispatcher {
  def process = {
    case (chat, VerbWithSaleID("restore", saleID, sender)) => {
      val conf = ConfigFactory.load()
      val consolePathOpt = if (conf.hasPath("restore-dispatcher.rails-console-path")) {
        Some(conf.getString("restore-dispatcher.rails-console-path"))
      } else {
        None
      }
      
      val isValidUser = if (conf.hasPath("authorized-users")) {
        conf.getStringList("authorized-users").asScala.find(adminAccount => sender.startsWith(adminAccount + "/")).isDefined
      } else {
        false  
      }

      val saleExistsOpt = consolePathOpt.map((consolePath: String) => {
        val checkSaleExistExpr = "(HypoOrder.find_by_sale_id " + saleID + ") != nil"
        (("echo " + checkSaleExistExpr) #| consolePath).lines.mkString.contains("true")
      })

      (consolePathOpt, isValidUser, saleExistsOpt) match {
        case (None, _, _) => chat.reply("restore-dispatcher.rails-console-path 沒設定好，無法使用此功能。")
        case (_, false, _) => chat.reply("你不是管理者，你壞壞。")
        case (Some(consolePath), true, Some(false)) => chat.reply("找不到 #" + saleID)
        case (Some(consolePath), true, Some(true)) => {
          val restoreExpression = "o=HypoOrder.find_by_sale_id " + saleID + ";b=o.to_book;b.save"
          val output = (("echo " + restoreExpression) #| consolePath).lines.mkString("\n")
          if (output.contains("=> true"))
            chat.reply("成功放回 #" + saleID)
          else
            chat.reply("好像有錯誤喔：\n" + output)
        }
      }
      None
    }
  }
}

class UnknowCommandDispatcher extends AnyRef with CommandDispatcher {
  def process = {
    case (chat, message) => {
      chat.reply("什麼是 \"" + message.getBody + "\"？")
      None
    }
  }
}