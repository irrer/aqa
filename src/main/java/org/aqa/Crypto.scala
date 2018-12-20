package org.aqa

import gnu.crypto.hash.HashFactory
import scala.util.Random
import java.security.InvalidParameterException
import gnu.crypto.cipher.IBlockCipher
import edu.umro.ScalaUtil.Trace

object Crypto extends Logging {

  /** Type of secure hash used. */
  private val DIGEST_NAME = "sha-512"

  /** Type of encryption to be used.  Rijndael is the same as AES but permits 256 bit keys where AES is only 128 bits. */
  private val cipherType = gnu.crypto.Registry.RIJNDAEL_CIPHER

  /** Minimum increment for size of message in bytes. */
  private val cipherBlockSize = 256 / 8

  /** Number of bytes used for encryption key. */
  private val cipherKeySize = 256 / 8

  private val rand = new Random

  /** Convert byte array to printable hex text */
  def byteArrayToHex(bytes: Array[Byte]): String = bytes.foldLeft("")((t, b) => t + b.formatted("%02x"))

  /** Convert string of hex characters to byte array. */
  def hexToByteArray(text: String): Array[Byte] = javax.xml.bind.DatatypeConverter.parseHexBinary(text)

  def secureHash(data: Array[Byte]): Array[Byte] = {
    val md = HashFactory.getInstance(DIGEST_NAME)
    md.update(data, 0, data.size)
    md.digest
  }

  /**
   * Calculate a secure hash of the given text.
   */
  def secureHash(text: String): String = byteArrayToHex(secureHash(text.getBytes))

  /**
   * Generate a random cryptographically secure hash value.
   */
  def randomSecureHash: String = {
    val rand = new Random
    val words = (0 until 100).map(i => rand.nextLong.toString)
    val text = words.foldLeft(System.currentTimeMillis.toString)((t, l) => t + l)
    secureHash(text)
  }

  /**
   * Create cipher that can be used to encrypt and decrypt.
   *
   * @param key: Secret password, must be 32 characters long.
   */
  def getCipher(key: Array[Byte]): IBlockCipher = {
    import gnu.crypto.cipher.IBlockCipher
    import gnu.crypto.cipher.BaseCipher
    import gnu.crypto.cipher.CipherFactory
    import java.util.HashMap

    if (key.size != cipherKeySize) throw new InvalidParameterException("Key must be " + cipherKeySize + " bytes but was actually " + key.size)

    val cipher = CipherFactory.getInstance(cipherType)

    val attributes = new HashMap[String, Object]()
    attributes.put(IBlockCipher.CIPHER_BLOCK_SIZE.toString, new Integer(cipherBlockSize))
    attributes.put(IBlockCipher.KEY_MATERIAL, key)
    cipher.init(attributes)

    cipher
  }

  def encrypt(clearText: String, cipher: IBlockCipher): String = {

    // prepend the text with its length and a blank.
    val fullText = clearText.size.toString.getBytes ++ " ".getBytes ++ clearText.getBytes

    val half = cipherBlockSize / 2 // characters per block
    val numBlock = (fullText.size / half) + 1
    val out = new Array[Byte](numBlock * cipherBlockSize)
    rand.nextBytes(out)

    for (b <- 0 until numBlock) {
      val block = new Array[Byte](cipherBlockSize)
      rand.nextBytes(block)
      val textToEncrypt = fullText.drop(half * b).take(b)
      (0 until textToEncrypt.size).map(i => block(i) = textToEncrypt(i))
      cipher.encryptBlock(block, 0, out, b * cipherBlockSize)
    }
    byteArrayToHex(out)
  }

  /** Convenience where only one encryption is done with the given key. */
  def encrypt(clearText: String, key: Array[Byte]): String = encrypt(clearText, getCipher(key))

  def decrypt(encryptedTextAsHex: String, cipher: IBlockCipher): String = {

    val encryptedText = hexToByteArray(encryptedTextAsHex)
    val j = encryptedText.size

    val clear = new Array[Byte](encryptedText.size)
    val numBlock = encryptedText.size / cipherBlockSize
    (0 until numBlock).map(b => cipher.decryptBlock(encryptedText, b * cipherBlockSize, clear, b * cipherBlockSize))
    val justText = (0 until numBlock).map(b => clear.drop(b * cipherBlockSize).take(cipherBlockSize / 2)).flatten.toArray
    val separator = justText.indexOf(' '.toByte)
    val lengthOfOriginalText = (new String(justText.take(separator))).toInt
    val originalText = justText.drop(separator + 1).take(lengthOfOriginalText)
    new String(originalText)
  }

  /** Convenience where only one decryption is done with the given key. */
  def decrypt(encryptedText: String, key: Array[Byte]): String = decrypt(encryptedText, getCipher(key))

}