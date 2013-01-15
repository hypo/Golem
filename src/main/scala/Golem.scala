package cc.hypo.Golem

import org.jivesoftware.smack._
import org.jivesoftware.smack.packet._
import com.typesafe.config._

class ConversationListener extends ChatManagerListener {
  override def chatCreated(chat: Chat, createdLocally: Boolean): Unit = {
    println(s"Chat created: " + chat)
    chat.addMessageListener(new SentenceListener(ConfigFactory.load))
  }
}

object Golem {
  def main(args: Array[String]): Unit = {
    val conf = ConfigFactory.load()
    if (!conf.hasPath("gtalk-username") || !conf.hasPath("gtalk-password")) {
      System.err.println("Need to specify gtalk-username and gtalk-password.")
      sys.exit
    }

    val gtalkUsername = conf.getString("gtalk-username")
    val gtalkPassword = conf.getString("gtalk-password")

    val connectionConfig = new ConnectionConfiguration("talk.google.com", 5222, "gmail.com")
    connectionConfig.setCompressionEnabled(true)
    connectionConfig.setSASLAuthenticationEnabled(true)

    val connection = new XMPPConnection(connectionConfig)
    connection.connect()
    connection.login(gtalkUsername, gtalkPassword)

    val chatManager = connection.getChatManager
    chatManager.addChatListener(new ConversationListener())

    while (true) {
      System.in.read
      Thread.sleep(100)
    }
  }
}

