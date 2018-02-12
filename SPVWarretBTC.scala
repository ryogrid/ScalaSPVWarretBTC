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
  var addFrom:NetAddr = null
  var nonce:Long = 0
  var userAgent:StringBuffer = null
  var startHeight:Int = 0
  var relay:Boolean = false
}

