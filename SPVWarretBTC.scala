import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.IOException
import java.net.Socket
import java.security.MessageDigest
import java.security.NoSuchAlgorithmException
import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.collection.JavaConversions._

class MessageHeader {
  var magic:Int = 0
  var commandName:Array[Char] = new Array[Char](12)
  var payloadSize:Int = 0
  var checksum:Array[Char] = new Array[Char](4)
}

class NetAddr {
  var services:Long = 0
  var ip:Array[Char] = new Array[Char](16)
  var port:Short = 0
}

class Version{
  var version:Int = 0
  var services:Long = 0
  var timestamp:Long = 0
  var addrRecv:NetAddr = null
  var addrFrom:NetAddr = null
  var nonce:Long = 0
//  var userAgent:StringBuffer = null
  var userAgent:Array[Char] = null
  var startHeight:Int = 0
  var relay:Boolean = false
  var bytes:Int = 86
}

class Verack{
  var commandName:String = "verack"
}

class MessageHandler(dummy:String) {

  var din:DataInputStream = null
  var dout:DataOutputStream = null

  // 補助コンストラクタ
  def this() {
    this ("dummy")

    var client:Socket = null
    try {
      client = new Socket("testnet-seed.bitcoin.jonasschnelli.ch", 18333)
    }catch {
      case e: IOException =>
        System.out.println(e)
    }

    try {
      din = new DataInputStream(client.getInputStream())
      dout = new DataOutputStream(client.getOutputStream())
    }catch {
      case e: IOException =>
        System.out.println(e)
    }
  }

//  def readHex(hex:String):String={
//  }

  def sha256(payload:Array[Byte]):String={
    var md: MessageDigest = null
    var sb: StringBuilder = null
    try {
      md = MessageDigest.getInstance("SHA-256")
    } catch {
      case e: NoSuchAlgorithmException =>
        e.printStackTrace()
    }
    var ret:Array[Byte] = md.update(payload)

    return ret
//    sb = new StringBuilder
//    for (b <- md.digest) {
//      val hex: String = String.format("%02x", b)
//      sb.append(hex)
//    }
//
//    return sb.toString()
  }

  def hash256(pyload:Array[Byte]):String={
    return sha256(sha256(payload))
  }


  def longToLittleNosin(value:Long):Long={
    val buf = ByteBuffer.allocate(8)
    buf.putLong(value)
    buf.flip()
    buf.order(ByteOrder.LITTLE_ENDIAN)
    return Long.parseUnsignedLong(String.valueOf(buf.getLong()))
  }

  def intToLittleNosin(value:Int):Int={
    val buf = ByteBuffer.allocate(4)
    buf.putInt(value)
    buf.flip()
    buf.order(ByteOrder.LITTLE_ENDIAN)
    return Integer.parseUnsignedInt(String.valueOf(buf.getInt()))
  }

  def shortToLittleNosin(value:Short):Short={
    val buf = ByteBuffer.allocate(2)
    buf.putShort(value)
    buf.flip()
    buf.order(ByteOrder.LITTLE_ENDIAN)
    return Short.parseUnsignedShort(String.valueOf(buf.getShort()))
  }

  def byteToLittleNosin(value:Byte):Byte={
    val buf = ByteBuffer.allocate(1)
    buf.putByte(value)
    buf.flip()
    buf.order(ByteOrder.LITTLE_ENDIAN)
    return Byte.parseUnsignedByte(String.valueOf(buf.getByte()))
  }

  def create_header(msg:Version, data:Array[Byte]):MessageHeader ={
    ret:MessageHeader = new MessageHeader()

    ret.magic = intToLittleNosin(0x0709110B)
    var cmd_name:Char[] = "version".ToCharArray()
    var cnt = 0
    for (ch <- cmd_name) {
      ret.commandName[cnt++] = ch
    }
    ret.userAgent = Byte[1]{byteToLittleNosin(0)}
    ret.payloadSize = intToLittleNosin(msg.bytes)
    var hash:Array[Byte] = hash256(String.valueOf(data))
    ret.checksum[0] = byteToLittleNosin(data[0])
    ret.checksum[1] = byteToLittleNosin(data[1])
    ret.checksum[2] = byteToLittleNosin(data[2])
    ret.checksum[3] = byteToLittleNosin(data[3])

    return ret
  }

  def read_header():MessageHeader={
    var ret = new MessageHeader()

    print(ret.commandName)
    return ret
  }

  def read_netaddr():NetAddr={
    return new NetAddr()
  }

  def read_version():Version={
    return new Version()
  }

  def read_verack():Verack={
    return new Verack()
  }

  def write_header(header:MessageHeader){
    print(new String(header.commandName))


  }

  def write_netaddr(addr:NetAddr) {

  }

  def write_version(ver:Version){

    var data:Array[Byte] = new Array[Byte]()
    write_header(create_header(ver, data))
  }

  def write_verack(vrk:Verack){

  }

  def withBitcoinConnection(){
    write_version()
    var is_version = false
    var is_verack = false
    while(is_version == false || is_verack == false){
      var header = read_header()
      cmd = new String(header.commandName)
      cmd match{
        case "version" =>
          is_version = true
          var ver:Version = read_version()
          write_verack()
        case "verack" =>
          is_verack = true
          var vack = read_verack()
      }
    }
  }

}

object Main{
  def main(args: Array[String]) {
    println("Hello, World")
    var msg_handler = new MessageHandler()
    msg_handler.withBitcoinConnection()
  }
}