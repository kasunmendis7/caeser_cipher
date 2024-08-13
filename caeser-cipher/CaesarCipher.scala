import scala.io.StdIn.readLine
object CaesarCipher {
  def encrypt(text: String, shift: Int): String = {
    val normalizedShift = shift % 26
    text.map { char =>
      if (char.isLetter) {
        val base = if (char.isUpper) 'A' else 'a'
        ((char - base + normalizedShift+26) % 26 + base).toChar
      } else {
        char
      }
    }
  }

  def decrypt(text: String, shift: Int): String = {
    val normalizedShift = shift % 26
    text.map { char =>
      if (char.isLetter) {
        val base = if (char.isUpper) 'A' else 'a'
        ((char - base - normalizedShift + 26) % 26 + base).toChar
      } else {
        char
      }
    }
  }

  def cipherFunction(text: String, shift: Int, operation: (String, Int) => String): String = {
    operation(text, shift)
  }

  def main(args: Array[String]): Unit = {
    print("Enter the message: ")
    val message = readLine()
    print("Enter the shift value: ")
    val shift = readLine().toInt

    val encryptedMessage = cipherFunction(message, shift, encrypt)
    println(s"Encrypted Message: $encryptedMessage")

    print("Press 1 to decrypt the message: ")
    val num = readLine().toInt
    if (num == 1) {
      val decryptedMessage = cipherFunction(encryptedMessage, shift, decrypt)
      println(s"Decrypted Message: $decryptedMessage")
    }
  }
}
