package cc.hypo.Golem

import org.jivesoftware.smack._
import org.jivesoftware.smack.packet._
import scala.sys.process._
import com.typesafe.config._

class SentenceListener extends MessageListener {
  val dispatchers = List(
    new RestoreCommandDispatcher(),
    new UnknowCommandDispatcher()
  )
  val composedDispatcher = dispatchers.map(_.process).reduceLeft((f1, f2) => f1 orElse f2)

  override def processMessage(chat: Chat, message: Message): Unit = {
    composedDispatcher(chat, message)
  }	
}

trait CommandDispatcher {
  def process: PartialFunction[(Chat, Message), Option[CommandDispatcher]]
}

object VerbWithSaleID {
  val VerbWithSaleIDPattern = """(?i)(\w+)\s*#?(\d+)""".r
  def unapply(m: Message) = m.getBody match {
    case VerbWithSaleIDPattern(verb, saleID) => Some((verb.toLowerCase, saleID))
    case _ => None
  }
}

class RestoreCommandDispatcher extends Object with CommandDispatcher {
  def process = {
    case (chat, VerbWithSaleID("restore", saleID)) => {      
      println("restoring saleID:" + saleID)
      val conf = ConfigFactory.load()
      val consolePathOpt = if (conf.hasPath("restore-dispatcher.rails-console-path")) {
        Some(conf.getString("restore-dispatcher.rails-console-path"))
      } else {
        None
      }
      
      val saleExistsOpt = consolePathOpt.map((consolePath: String) => {
        val checkSaleExistExpr = "(HypoOrder.find_by_sale_id " + saleID + ") != nil"
        (("echo " + checkSaleExistExpr) #| consolePath).lines.mkString.contains("true")
      })
      
      (consolePathOpt, saleExistsOpt) match {
        case (None, _) => chat.sendMessage("restore-dispatcher.rails-console-path 沒設定好，無法使用此功能。")
        case (Some(consolePath), Some(false)) => chat.sendMessage("找不到 #" + saleID)
        case (Some(consolePath), Some(true)) => {
          val restoreExpression = "o=HypoOrder.find_by_sale_id " + saleID + ";b=o.to_book;b.save"
          val output = (("echo " + checkSaleExistExpr) #| consolePath).lines.mkString("\n")
          if (output.contains("=> true"))
            chat.sendMessage("成功放回 #" + saleID)
          else
            chat.sendMessage("好像有錯誤喔：\n" + output)
        }
      }
      None
    }
  }
}

class UnknowCommandDispatcher extends Object with CommandDispatcher {
  def process = {
    case (chat, message) => {
      chat.sendMessage("什麼是 \"" + message.getBody + "\"？")
      None
    }
  }
}