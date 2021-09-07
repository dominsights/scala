import scala.util.Try
import java.util.Random
val hostname = "localhost"
val port = "8080"
def renderHTML(page: String) = println(page)

class Connection {
    def get(url: String): String = 
        val random = new Random(System.nanoTime)
        if random.nextBoolean then "<html>...</html>"
        else throw new RuntimeException("Connection interrupted")
}

object HttpService {
    val random = new Random(System.nanoTime)
    def getConnection(host: String, port: String): Connection =
        if random.nextBoolean then Connection()
        else throw new RuntimeException("Someone else took the port")
}

// if you get the html page from the connection, print it to the console i.e. call renderHTML

val httpService = HttpService
val connection = Try(httpService.getConnection(hostname, port))
val page = connection.flatMap(c => Try(c.get(s"${hostname}:${port}")))
page.foreach(renderHTML)

class ConnectionV2 {
    def get(url: String): String = 
        val random = new Random(System.nanoTime)
        if random.nextBoolean then "<html>...</html>"
        else throw new RuntimeException("Connection interrupted")
    def getSafe(url: String): Try[String] = 
        Try(get(url))
}

object HttpServiceV2 {
    val random = new Random(System.nanoTime)
    def getConnection(host: String, port: String): ConnectionV2 =
        if random.nextBoolean then ConnectionV2()
        else throw new RuntimeException("Someone else took the port")
    def getConnectionSafe(host: String, port: String): Try[ConnectionV2] =
        Try(getConnection(host, port))
}

// shorthand version

HttpServiceV2.getConnectionSafe(hostname, port)
    .flatMap(connection => connection.getSafe(s"${hostname}:${port}"))
    .foreach(renderHTML)

// for comprehension

for {
    connection <- HttpServiceV2.getConnectionSafe(hostname, port)
    html <- connection.getSafe("/home")
} renderHTML(html)


