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

    // try to establish a connection, if so - print the connect method
}