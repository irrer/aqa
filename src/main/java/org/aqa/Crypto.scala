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

  private def emptyBlock = new Array[Byte](cipherBlockSize)

  private def makeRandBlock = {
    val r = emptyBlock
    rand.nextBytes(r)
    r
  }

  /** Make a random cipher key of the size required by this service. */
  def makeRandomCipherKey: String = byteArrayToHex(makeRandBlock)

  /**
   * Create cipher that can be used to encrypt and decrypt.
   *
   * @param key: Secret password, original value must be 32 characters long
   */
  def getCipher(keyAsHex: String): IBlockCipher = {
    import gnu.crypto.cipher.IBlockCipher
    import gnu.crypto.cipher.BaseCipher
    import gnu.crypto.cipher.CipherFactory
    import java.util.HashMap

    val key = hexToByteArray(keyAsHex)
    if (key.size != cipherKeySize) throw new InvalidParameterException("Key must be " + cipherKeySize + " bytes but was actually " + key.size)

    val cipher = CipherFactory.getInstance(cipherType)

    val attributes = new HashMap[String, Object]()
    attributes.put(IBlockCipher.CIPHER_BLOCK_SIZE.toString, new Integer(cipherBlockSize))
    attributes.put(IBlockCipher.KEY_MATERIAL, key)
    cipher.init(attributes)

    cipher
  }

  /**
   * Copy all bytes from source into destination starting at the destination offset.
   *
   * This is necessarily mutable, but that is due to the support functions used.
   */
  private def byteCopy(src: Array[Byte], dest: Array[Byte], destOffset: Int) = {
    src.indices.map(i => dest(destOffset + i) = src(i))
  }

  /**
   * Encrypt and produce a string that is a string representation of the hex output.
   *
   * The encryption algorithm requires that the input be broken into fixed sized blocks,
   * each of which is encrypted separately.  To make each block unique, the first half of
   * each is filled with the input text, and the second half is randomized.
   *
   * @param clearText: Text to be encrypted.
   *
   * @param cipher: Cipher to be used.
   */
  def encryptWithNonce(clearText: String, cipher: IBlockCipher): String = {

    // prepend the text with its length and a blank.
    val fullText = clearText.size.toString.getBytes ++ " ".getBytes ++ clearText.getBytes
    val half = cipherBlockSize / 2 // characters per block

    def crpt(in: Array[Byte]) = {
      val out = emptyBlock
      cipher.encryptBlock(in, 0, out, 0)
      out
    }

    val numBlock = (fullText.size / half) + 1
    val out = new Array[Byte](numBlock * cipherBlockSize)
    rand.nextBytes(out)

    val textChunks = (0 until numBlock).map(b => fullText.drop(half * b).take(half))

    val chunkAndRand = textChunks.map(chunk => (chunk ++ makeRandBlock).take(cipherBlockSize))

    val allChunksEncrypted = chunkAndRand.map(car => crpt(car)).flatten

    byteArrayToHex(allChunksEncrypted.toArray)
  }

  /** Convenience where only one encryption is done with the given key. */
  def encryptWithNonce(clearText: String, key: String): String = encryptWithNonce(clearText, getCipher(key))

  /**
   * Decrypt text encrypted by <code>encrypt</code>.
   */
  def decryptWithNonce(encryptedTextAsHex: String, cipher: IBlockCipher): String = {

    try {
      val encryptedText = hexToByteArray(encryptedTextAsHex)
      val j = encryptedText.size

      val clear = new Array[Byte](encryptedText.size)
      val numBlock = encryptedText.size / cipherBlockSize
      (0 until numBlock).map(b => cipher.decryptBlock(encryptedText, b * cipherBlockSize, clear, b * cipherBlockSize))
      val justText = (0 until numBlock).map(b => clear.drop(b * cipherBlockSize).take(cipherBlockSize / 2)).flatten.toArray
      val separator = justText.indexOf(' '.toByte)
      val intText = new String(justText.take(separator))
      if (intText.matches("[0-9][0-9]*")) {
        val lengthOfOriginalText = intText.toInt
        val originalText = justText.drop(separator + 1).take(lengthOfOriginalText)
        new String(originalText)
      } else
        "Could not decrypt"
    } catch {
      case t: Throwable => {
        logger.warn("Failure to decrypt string: " + encryptedTextAsHex + " : " + fmtEx(t))
        "decryption failure"
      }
    }
  }

  /** Convenience where only one decryption is done with the given key. */
  def decryptWithNonce(encryptedText: String, key: String): String = decryptWithNonce(encryptedText, getCipher(key))

  /**
   * Encrypt the given text (without nonce) and then calculate and return a secure hash of the result.
   */
  def encryptAndHash(text: String, cipher: IBlockCipher): String = {
    val numBlock = (text.size / cipherBlockSize) + 1
    val empty = new String(Array.fill(cipherBlockSize)(0.asInstanceOf[Byte]))
    val blocksConcatenated = (text + empty).take(numBlock * cipherBlockSize).getBytes
    val encrypted = Array.fill(numBlock * cipherBlockSize)(0.asInstanceOf[Byte])

    (0 until numBlock).map(b => cipher.encryptBlock(blocksConcatenated, b * cipherBlockSize, encrypted, b * cipherBlockSize))

    val hash = byteArrayToHex(secureHash(encrypted))
    hash
  }

  def encryptAndHash(text: String, key: String): String = encryptAndHash(text, getCipher(key))

}