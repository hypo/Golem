package cc.hypo.Golem
import cc.hypo.utilities.base64._
import scala.slick.driver.MySQLDriver.simple._
import org.newsclub.net.mysql.AFUNIXDatabaseSocketFactory

// import Database.threadLocalSession
import java.sql.Timestamp
import java.util.zip._

object Zlib {
  def inflate(bytes: Array[Byte]): Array[Byte] = {
    val inflater = new Inflater
    inflater.setInput(bytes, 0, bytes.length)

    var decompressedBytes = Array[Byte]()

    while (!inflater.finished) {
      val buffer = new Array[Byte](1024 * 10)
      val length = inflater.inflate(buffer)
      decompressedBytes = decompressedBytes ++ buffer.take(length)
    }
    decompressedBytes
  }
}

case class Book(id: Option[Int], 
                title: Option[String], 
                data: Option[String], 
                bookType: Option[String],
                user: Option[String],
                photoSource: Option[String],
                createdAt: Option[Timestamp],
                updatedAt: Option[Timestamp],
                sourceUser: Option[String],
                backend: Option[String] = Some("tw"),
                facebookEmail: Option[String],
                promoCodeId: Option[Int]
                )

object Books extends Table[Book]("books") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def title = column[Option[String]]("title")
  def data = column[Option[String]]("data")
  def bookType = column[Option[String]]("book_type")
  def user = column[Option[String]]("user")
  def photoSource = column[Option[String]]("photo_source")
  def createdAt = column[Option[Timestamp]]("created_at")
  def updatedAt = column[Option[Timestamp]]("updated_at")
  def sourceUser = column[Option[String]]("source_user")
  def backend = column[Option[String]]("backend")
  def facebookEmail = column[Option[String]]("facebook_email")
  def promoCodeID = column[Option[Int]]("promo_code_id")

  def * = id.? ~ 
          title ~ 
          data ~ 
          bookType ~ 
          user ~ 
          photoSource ~ 
          createdAt ~ 
          updatedAt ~ 
          sourceUser ~ 
          backend ~ 
          facebookEmail ~ 
          promoCodeID <> (Book, Book.unapply _)
  
}

case class HypoOrder( id: Option[Int], 
                      data: Option[String],
                      bookType: Option[String],
                      user: Option[String],
                      photoSource: Option[String],
                      sourceUser: Option[String],
                      saleID: Option[String],
                      createdAt: Option[Timestamp],
                      updatedAt: Option[Timestamp],
                      address: Option[String],
                      confirmed: Option[Boolean],
                      status: Option[String] = Some("pending"),
                      quantity: Option[Int] = Some(1),
                      token: Option[String],
                      backend: Option[String] = Some("tw"),
                      facebookEmail: Option[String],
                      ticketLite: Option[Boolean]
                      ) {
  def toBook: (Book) = 
    Book( id = None, 
          title = None, 
          data = data, 
          bookType = bookType, 
          user = user, 
          photoSource = photoSource, 
          createdAt = createdAt, 
          updatedAt = updatedAt, 
          sourceUser = sourceUser,
          backend = backend,
          facebookEmail = facebookEmail,
          promoCodeId = None)

  def extractedData: Option[String] = data.flatMap(d =>
    if (d.startsWith("{")) Some(d)
    else {
      for (decoded <- Base64.decode(d).toOption) 
      yield new String(Zlib.inflate(decoded.toArray), "UTF-8")
    })
    
  
}

object HypoOrders extends Table[HypoOrder]("hypo_orders") {
  def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
  def data = column[Option[String]]("data")
  def bookType = column[Option[String]]("book_type")
  def user = column[Option[String]]("user")
  def photoSource = column[Option[String]]("photo_source")
  def sourceUser = column[Option[String]]("source_user")
  def saleID = column[Option[String]]("sale_id")
  def createdAt = column[Option[Timestamp]]("created_at")
  def updatedAt = column[Option[Timestamp]]("updated_at")
  def address = column[Option[String]]("address")
  def confirmed = column[Option[Boolean]]("confirmed")
  def status = column[Option[String]]("status")
  def quantity = column[Option[Int]]("quantity")
  def token = column[Option[String]]("token")
  def backend = column[Option[String]]("backend")
  def facebookEmail = column[Option[String]]("facebook_email")
  def ticketLite = column[Option[Boolean]]("ticket_lite")

  def * = id.? ~ 
          data ~ 
          bookType ~ 
          user ~ 
          photoSource ~ 
          sourceUser ~ 
          saleID ~
          createdAt ~ 
          updatedAt ~ 
          address ~
          confirmed ~
          status ~
          quantity ~
          token ~
          backend ~ 
          facebookEmail ~ 
          ticketLite <> (HypoOrder, HypoOrder.unapply _)
}

case class EditorDatabase(url: String, user: String, password: String, socketFile: String) {
  val prop = new java.util.Properties()
  prop.put("socketFactory", classOf[AFUNIXDatabaseSocketFactory].getName)
  prop.put("junixsocket.file", socketFile)

  val db = Database.forURL(url = url, user = user, password = password, prop = prop, driver = "com.mysql.jdbc.Driver")
  def orderForSaleID(saleID: String) = db withSession { implicit session: Session =>
    val q1 = for { o <- HypoOrders if o.saleID === saleID } yield o
    q1.firstOption    
  }

  def insertBook(b: Book) = db withSession { implicit session: Session =>
    Books.insertAll(b)
  }
}