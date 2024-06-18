/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa

import gnu.crypto.cipher.IBlockCipher
import gnu.crypto.hash.HashFactory

import java.security.InvalidParameterException
import scala.util.Random

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

  /**
    * Convert string of hex characters to byte array.  There is a standard java
    *  library javax.xml.bind.DatatypeConverter.parseHexBinary that does this but
    *  in Java 12 it requires pulling in an extra jar which creates an extra dependency.
    *  This is only a few lines of code.
    */
  def hexToByteArray(text: String): Array[Byte] = {

    def oneByte(array: Array[Byte], index: Int): Array[Byte] = {
      array :+ java.lang.Integer.parseInt(text.substring(index, index + 2), 16).toByte
    }

    try {
      val array = text.indices.filter(i => (i % 2) == 0).foldLeft(Array[Byte]())(oneByte)
      array
    } catch {
      case t: Throwable =>
        val msg = "unable to convert HEX string '" + text + "' to bytes: " + fmtEx(t)
        logger.error(msg)
        throw new IllegalArgumentException(msg)
    }
  }

  /**
    * Construct a secure hash from the given data.  Secure in this context means it
    * is not computationally viable to reverse.
    *
    * @param data Source data.
    * @return Secure hash of data.
    */
  def secureHash(data: Array[Byte]): Array[Byte] = {
    val md = HashFactory.getInstance(DIGEST_NAME)
    md.update(data, 0, data.length)
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
    val words = (0 until 100).map(_ => rand.nextLong.toString)
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
    * @param keyAsHex: Secret password, original value must be 32 characters long
    */
  def getCipher(keyAsHex: String): IBlockCipher = {
    import gnu.crypto.cipher.CipherFactory
    import gnu.crypto.cipher.IBlockCipher

    val key = hexToByteArray(keyAsHex)
    if (key.length != cipherKeySize) throw new InvalidParameterException("Key must be " + cipherKeySize + " bytes but was actually " + key.length)

    val cipher = CipherFactory.getInstance(cipherType)

    val attributes = new java.util.HashMap[String, Object]()
    val size: Object = cipherBlockSize.asInstanceOf[Object]
    attributes.put(IBlockCipher.CIPHER_BLOCK_SIZE, size)
    attributes.put(IBlockCipher.KEY_MATERIAL, key)
    cipher.init(attributes)

    cipher
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
    val fullText = clearText.length.toString.getBytes ++ " ".getBytes ++ clearText.getBytes
    val half = cipherBlockSize / 2 // characters per block

    def crp(in: Array[Byte]) = {
      val out = emptyBlock
      cipher.encryptBlock(in, 0, out, 0)
      out
    }

    val numBlock = (fullText.length / half) + 1
    val out = new Array[Byte](numBlock * cipherBlockSize)
    rand.nextBytes(out)

    val textChunks = (0 until numBlock).map(b => fullText.slice(half * b, half * b + half))

    val chunkAndRand = textChunks.map(chunk => (chunk ++ makeRandBlock).take(cipherBlockSize))

    val allChunksEncrypted = chunkAndRand.flatMap(car => crp(car))

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

      val clear = new Array[Byte](encryptedText.length)
      val numBlock = encryptedText.length / cipherBlockSize
      (0 until numBlock).foreach(b => cipher.decryptBlock(encryptedText, b * cipherBlockSize, clear, b * cipherBlockSize))
      val justText = (0 until numBlock).flatMap(b => clear.slice(b * cipherBlockSize, b * cipherBlockSize + cipherBlockSize / 2)).toArray
      val separator = justText.indexOf(' '.toByte)
      val intText = new String(justText.take(separator))
      if (intText.matches("[0-9]+")) {
        val lengthOfOriginalText = intText.toInt
        val originalText = justText.slice(separator + 1, separator + 1 + lengthOfOriginalText)
        new String(originalText)
      } else
        "Could not decrypt"
    } catch {
      case t: Throwable =>
        logger.warn("Failure to decrypt string: " + encryptedTextAsHex + " : " + fmtEx(t))
        "decryption failure"
    }
  }

  /** Convenience where only one decryption is done with the given key. */
  def decryptWithNonce(encryptedText: String, key: String): String = decryptWithNonce(encryptedText, getCipher(key))

  /**
    * Encrypt the given text (without nonce) and then calculate and return a secure hash of the result.
    */
  private def encryptAndHash(text: String, cipher: IBlockCipher): String = {
    val numBlock = (text.length / cipherBlockSize) + 1
    val empty = new String(Array.fill(cipherBlockSize)(0.asInstanceOf[Byte]))
    val blocksConcatenated = (text + empty).take(numBlock * cipherBlockSize).getBytes
    val encrypted = Array.fill(numBlock * cipherBlockSize)(0.asInstanceOf[Byte])

    (0 until numBlock).foreach(b => cipher.encryptBlock(blocksConcatenated, b * cipherBlockSize, encrypted, b * cipherBlockSize))

    val hash = byteArrayToHex(secureHash(encrypted))
    hash
  }

  def encryptAndHash(text: String, key: String): String = encryptAndHash(text, getCipher(key))

}
