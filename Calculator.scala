import scala.collection.mutable.Map
import scala.io.StdIn.readLine

object Calculator {
    def main(args: Array[String]): Unit = {
        var memory: Map[String, Double] = Map()
        var loop = true
        while (loop) {
            val input = readLine("> ")
            if (input == null || input == "") {
                loop = false
            }
            if (loop) {
                val tokensResult: Either[String, Array[Token]] =
                    (new Lexer(input.trim)).lex
                tokensResult match {
                    case Left(message) => println(message)
                    case Right(tokens) => {
                        for (token <- tokens) { print(token.toString) }
                        println("")
                        val nodeResult: Either[String, ASTNode] =
                            (new Parser(tokens)).parse
                        nodeResult match {
                            case Left(message) => println(message)
                            case Right(node) => {
                                println(node.toString)
                                println(node.evaluate(memory))
                            }
                        }
                    }
                }
            }
        }
    }
}
