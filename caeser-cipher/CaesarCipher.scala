import scala.io.StdIn.readLine;
object CaesarCipher {
  def encrypt(text: String, shift: Int): String = {
    text.map{char=>
      if(char.isLetter){
        val base = if(char.isUpper) 'A' else 'a'
        ((char - base + shift)%26+base).toChar
      }else{
        char
      }
    }
  }

  def decrypt(text: String, shift: Int): String = {
    text.map { char =>
      if (char.isLetter) {
        val base = if (char.isUpper) 'A' else 'a'
        ((char - base - shift + 27) % 26 + base).toChar
      } else {
        char
      }
    }
  }

  def cipherFunction(text: String, shift: Int, operation: (String, Int)=>String): String = {
    operation(text, shift);
  }

  def main(args: Array[String]): Unit = {
    print("Enter the message: ")
    var message = readLine();
    print("Enter the shift value: ")
    var shift = readLine().toInt;
    
    val encryptedMessage = cipherFunction(message, shift, encrypt)
    println(s"Encrypted Message: $encryptedMessage")

    val decryptedMessage = cipherFunction(message, shift, decrypt)
    println(s"Decrypted Message: $decryptedMessage")
  }
}
