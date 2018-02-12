import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.IOException
import java.net.Socket
import java.security.MessageDigest
import java.security.NoSuchAlgorithmException
import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.collection.JavaConversions._

class MessageHeader(
  var magic: Int = 0,
  var commandName: Array[Byte] = new Array[Byte](12),
  var payloadSize: Int = 0,
  var checksum: Array[Byte] = new Array[Byte](4)
)

class NetAddr(
  var services: Long = 0,
  var ip: Array[Byte] = new Array[Byte](16),
  var port: Short = 0
)

class Version(
  var version: Int = 0,
  var services: Long = 0,
  var timestamp: Long = 0,
  var addrRecv: NetAddr = null,
  var addrFrom: NetAddr = null,
  var nonce: Long = 0,
  var userAgent: Array[Char] = null,
  var startHeight:Int = 0,
  var relay: Boolean = false,
  var bytes: Int = 86
)

class Verack(var commandName: String = "verack")

class MessageHandler(dummy:String = "dummy") {
  val client: Socket = new Socket("testnet-seed.bitcoin.jonasschnelli.ch", 18333)
  val din: DataInputStream = new DataInputStream(client.getInputStream())
  var dout: DataOutputStream = new DataOutputStream(client.getOutputStream())

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
    md.digest()
  }

  def hash256(payload:Array[Byte]):Array[Byte]={
    sha256(sha256(payload))
  }


  def longToLittleNosin(value:Long):Long={
    val buf = ByteBuffer.allocate(8)
    buf.putLong(java.lang.Long.parseUnsignedLong(String.valueOf(value)))
    buf.flip()
    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.getLong()
  }

  def intToLittleNosin(value:Int):Int={
    val buf = ByteBuffer.allocate(4)
    buf.putInt(Integer.parseUnsignedInt(String.valueOf(value)))
    buf.flip()
    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.getInt()
  }

  def shortToLittleNosin(value:Short):Short={
    val buf = ByteBuffer.allocate(2)
    buf.putShort(Integer.parseUnsignedInt(String.valueOf(value)).asInstanceOf[Short])
    buf.flip()
    buf.order(ByteOrder.LITTLE_ENDIAN)
    buf.getShort()
  }

  def byteToLittleNosin(value:Byte):Byte={
    val buf = ByteBuffer.allocate(1)
    buf.put(value)
    buf.flip()
    buf.order(ByteOrder.LITTLE_ENDIAN)
    Integer.parseUnsignedInt(String.valueOf(buf.get())).asInstanceOf[Byte]
  }

  def create_header(msg: Version, data: Array[Byte]): MessageHeader = {
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
    ret.checksum(0) = hash(0)
    ret.checksum(1) = hash(1)
    ret.checksum(2) = hash(2)
    ret.checksum(3) = hash(3)

    ret
  }

  def read_header(): MessageHeader = {
    var ret = new MessageHeader()

    din.readInt()
    var cmd_name:Array[Byte] = new Array[Byte](12)
    din.read(cmd_name, 0, 12)
    ret.commandName = cmd_name
    din.read(new Array[Byte](4), 0, 4)
    ret
  }

  def read_netaddr(): NetAddr = {
    new NetAddr()
  }

  def read_version(): Version = {
    new Version()
  }

  def read_verack(): Verack = {
    new Verack()
  }

  def write_header(header:MessageHeader): Unit = {
    dout.writeInt(header.magic)
    dout.write(header.commandName, 0, 12)
    dout.writeInt(header.payloadSize)
    dout.write(header.checksum, 0, 4)
  }

  def write_netaddr(buf:ByteBuffer): Unit =  {
    buf.putLong(longToLittleNosin(1))
    for(ip <- Array(0,0,0,0,0,0,0,0,0,0,255,255,127,0,0,1)){
      buf.put(ip.asInstanceOf[Byte])
    }
    buf.putShort(8333)
  }

  def write_version(ver:Version): Unit = {
    var buf = ByteBuffer.allocate(86)

    buf.putInt(intToLittleNosin(70015))
    buf.putLong(longToLittleNosin(1))
    buf.putLong(longToLittleNosin((System.currentTimeMillis()/1000).asInstanceOf[Long]))
    write_netaddr(buf)
    write_netaddr(buf)
    buf.putLong(longToLittleNosin(0))
    buf.put(byteToLittleNosin(0))
    buf.putInt(intToLittleNosin(0))
    buf.put(byteToLittleNosin(0))

    var ver_arr = buf.array()
    write_header(create_header(ver, ver_arr))
    dout.write(ver_arr, 0, ver_arr.length)
  }

  def write_verack(): Unit = {
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
    header.checksum(0) = shortToLittleNosin(0x5d).asInstanceOf[Byte]
    header.checksum(1) = shortToLittleNosin(0xf6).asInstanceOf[Byte]
    header.checksum(2) = shortToLittleNosin(0xe0).asInstanceOf[Byte]
    header.checksum(3) = shortToLittleNosin(0xe2).asInstanceOf[Byte]

    write_header(header)
  }

  def withBitcoinConnection(): Unit = {
    var ver:Version = new Version()
    write_version(ver)
    println("send version")
    var is_version = false
    var is_verack = false
    while(is_version == false || is_verack == false){
      var header = read_header()
      var cmd_char:Array[Char] = new Array[Char](12)
      var cnt:Int = 0
      for(ch <- header.commandName){
        cmd_char(cnt) = ch.asInstanceOf[Char]
        cnt += 1
      }
      var cmd = new String(cmd_char)
      println("recv " + cmd)
      if(cmd.equals("version")) {
        is_version = true
        var ver: Version = read_version()
        write_verack()
      }else if(cmd.equals("verack")){
          is_verack = true
          var vack = read_verack()
      }
    }
  }

}

object Main{
  def main(args: Array[String]) {
    var msg_handler = new MessageHandler()
    msg_handler.withBitcoinConnection()
  }
}
