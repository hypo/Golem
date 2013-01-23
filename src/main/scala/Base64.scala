package cc.hypo.utilities.base64
import scala.util.{ Try, Success, Failure }

class InvalidBase64FormatException extends Exception("Not a valid base64 format")

object Base64 {
  val encodeTable = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  val decodeTable = Array(-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1, -1, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1, -1, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1)

  def encode(fromBytes: Seq[Byte]) : String = {
    var index = 0
    val inputLength = fromBytes.length
    val result = new StringBuilder
    var inserted = 0

    while (index < inputLength) {
      val b1: Int = 0xFF & fromBytes(index)
      val b2: Int = 0xFF & (if (index + 1 >= inputLength) 0 else fromBytes(index + 1))
      val b3: Int = 0xFF & (if (index + 2 >= inputLength) 0 else fromBytes(index + 2))

      val c1 = encodeTable.charAt(b1 >> 2)
      val c2 = encodeTable.charAt(((b1 & 0x03) << 4) | (b2 >> 4))
      val c3 = (if (index + 1 >= inputLength) '=' else encodeTable.charAt(((b2 & 0x0F) << 2) | (b3 >> 6)))
      val c4 = (if (index + 2 >= inputLength) '=' else encodeTable.charAt(b3 & 0x3F))
      result.append(c1)
      result.append(c2)
      result.append(c3)
      result.append(c4)

      inserted += 4
      if (inserted % 76 == 0) result.append('\n')
      index += 3
    }
    result.toString
  }

  def decode(src: String) : Try[Seq[Byte]] = {
    var index = 0
    val srcLength = src.length
    var result = scala.collection.mutable.ArrayBuffer[Byte]()

    val CHAR_1 = 0
    val CHAR_2 = 1
    val CHAR_3 = 2
    val CHAR_4 = 3
    val FINISH = 5

    var state = CHAR_1
    var b = 0

    while (index < srcLength) {
      val c = src.charAt(index)

      if (c >= 128) throw new InvalidBase64FormatException

      val v = decodeTable(c)

      if (c == '\n') {
        /* Skip */
      } else if (c == '=') { /* Padding */
        if (state == CHAR_3) {
          state = CHAR_4
        } else if (state == CHAR_4) {
          state = FINISH
        } else {
          throw new InvalidBase64FormatException
        }
      } else if (state == FINISH) {
        // skip the characters or throw exception?

      } else if (v < 0) {
        throw new InvalidBase64FormatException /* invalid character */
      } else if (state == CHAR_1) {
        b = v << 2
        state = CHAR_2
      } else if (state == CHAR_2) {
        result += (b | (v >> 4)).toByte // flush the byte1
        b = (v & 0x0F) << 4
        state = CHAR_3
      } else if (state == CHAR_3) {
        result += (b | (v >> 2)).toByte
        b = (v & 0x03) << 6
        state = CHAR_4
      } else if (state == CHAR_4) {
        result += (b | v).toByte
        state = CHAR_1
      } else {
        throw new InvalidBase64FormatException // or just ignore?
      }
      index += 1
    }
    Success(result.toSeq)
  }
}
