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
  var commandName:Array[Byte] = new Array[Byte](12)
  var payloadSize:Int = 0
  var checksum:Array[Byte] = new Array[Byte](4)
}

class NetAddr {
  var services:Long = 0
  var ip:Array[Byte] = new Array[Byte](16)
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

  def sha256(payload:Array[Byte]):Array[Byte]={
    var md: MessageDigest = null
    var sb: StringBuilder = null
    try {
      md = MessageDigest.getInstance("SHA-256")
    } catch {
      case e: NoSuchAlgorithmException =>
        e.printStackTrace()
    }
    md.update(payload)
    var ret:Array[Byte] = md.digest()

    return ret
//    sb = new StringBuilder
//    for (b <- md.digest) {
//      val hex: String = String.format("%02x", b)
//      sb.append(hex)
//    }
//
//    return sb.toString()
  }

  def hash256(payload:Array[Byte]):Array[Byte]={
    return sha256(sha256(payload))
  }


  def longToLittleNosin(value:Long):Long={
    val buf = ByteBuffer.allocate(8)
    buf.putLong(value)
    buf.flip()
    buf.order(ByteOrder.LITTLE_ENDIAN)
    return java.lang.Long.parseUnsignedLong(String.valueOf(buf.getLong()))
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
    return Integer.parseUnsignedInt(String.valueOf(buf.getShort())).asInstanceOf[Short]
  }

  def byteToLittleNosin(value:Byte):Byte={
    val buf = ByteBuffer.allocate(1)
    buf.put(value)
    buf.flip()
    buf.order(ByteOrder.LITTLE_ENDIAN)
    return Integer.parseUnsignedInt(String.valueOf(buf.get())).asInstanceOf[Byte]
  }

  def create_header(msg:Version, data:Array[Byte]):MessageHeader ={
    var ret:MessageHeader = new MessageHeader()

    ret.magic = intToLittleNosin(0x0709110B)
    var cmd_name:Array[Char] = "version".toCharArray()
    var cnt = 0
    for (ch <- cmd_name) {
      ret.commandName(cnt) = ch.asInstanceOf[Byte]
      cnt += 1
    }
   // ret.userAgent = new Array[Byte]{byteToLittleNosin(0)}
    ret.payloadSize = intToLittleNosin(msg.bytes)
    var hash:Array[Byte] = hash256(data)
    ret.checksum(0) = byteToLittleNosin(hash(0))
    ret.checksum(1) = byteToLittleNosin(hash(1))
    ret.checksum(2) = byteToLittleNosin(hash(2))
    ret.checksum(3) = byteToLittleNosin(hash(3))

    return ret
  }

  def read_header():MessageHeader={
    var ret = new MessageHeader()

 //   print(ret.commandName)
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

    dout.writeInt(header.magic)
    dout.write(header.commandName, 0, 12)
    dout.writeInt(header.payloadSize)
    dout.write(header.checksum, 0, 4)
  }

  def write_netaddr(buf:ByteBuffer) {
    buf.writeLong(longToLittleNosin(1))
    for(ip <- Array(0,0,0,0,0,0,0,0,0,0,255,255,127,0,0,1)){
      buf.writeByte(ip)
    }
    buf.writeShort(8333)
  }

  def write_version(ver:Version){
    var buf = ByteBuffer.allocate(86)

    buf.writeInt(intToLittleNosin(70015))
    buf.writeLong(longToLittleNosin(1))
    buf.writeLong(longToLittleNosin(((System.currentTimeMillis() / 1000L).asInstanceOf[Int])))
    write_netaddr(buf)
    write_netaddr(buf)
    buf.writeLong(longToLittleNosin(0))
    buf.write(byteToLittleNosin(0))
    buf.writeInt(intToLittleNosin(0))
    buf.write(byteToLittleNosin(0))

    var ver_arr = buf.array()
    write_header(create_header(ver, ver_arr))
    dout.write(ver_arr, 0, ver_arr.length)
  }

  def write_verack(){
    var header:MessageHeader = new MessageHeader()

    header.magic = intToLittleNosin(0x0709110B)
    var cmd_name:Array[Char] = "verack".toCharArray()
    var cnt = 0
    for (ch <- cmd_name) {
      header.commandName(cnt) = ch.asInstanceOf[Byte]
      cnt += 1
    }
    // ret.userAgent = new Array[Byte]{byteToLittleNosin(0)}
    header.payloadSize = intToLittleNosin(0)
    var hash:Array[Byte] = hash256(data)
    header.checksum(0) = byteToLittleNosin(0x5d)
    header.checksum(1) = byteToLittleNosin(0xf6)
    header.checksum(2) = byteToLittleNosin(0xe0)
    header.checksum(3) = byteToLittleNosin(0xe2)

    write_header(header)
  }

  def withBitcoinConnection(){
    var ver:Version = new Version()
    write_version(ver)
    println("send version")
    var is_version = false
    var is_verack = false
    while(is_version == false || is_verack == false){
      var header = read_header()
      var cmd = new String(header.commandName)
      println("recv " + cmd)
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