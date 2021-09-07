import scala.util.Random

val config: Map[String, String] = Map(
    // fetched from elsewhere
    "host" -> "176.45.36.1",
    "port" -> "80"
)

class Connection {
    def connect = "Connected" // connect to some server
}
object Connection {
    val random = new Random(System.nanoTime())
    def apply(host: String, port: String): Option[Connection] =
        if random.nextBoolean then Some(new Connection)
        else None
}

// with flatmap and map

val host = config.get("host")
val port = config.get("port")

val connection = host.flatMap(h => port.flatMap(p => Connection(h, p)))
val connectionStatus = connection.map(c => c.connect)
connectionStatus.foreach(println)

// alternative with for comprehensions

val forConnectionStatus = for
    host <- config.get("host")
    port <- config.get("port")
    connection <- Connection(host, port)
yield connection.connect

forConnectionStatus.foreach(println)


