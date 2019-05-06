import scala.collection.mutable.ArrayBuffer;
import scala.util.{Either, Left, Right};

class Lexer(val source: String) {
    val length = source.length
    var start = 0
    var current = 0
    val constants: Array[String] = Array("pi", "e")
    val functions: Array[String] = Array("acos", "asin", "atan", "cos", "exp", "log", "sin", "tan")

    def lex(): Either[String, Array[Token]] = {
        var tokens = new ArrayBuffer[Token]()
        current = 0
        start = 0
        while (current < length) {
            skipWhitespace()
            if (current < length) {
                start = current
                val tokenResult = nextToken
                tokenResult match {
                    case Right(token) => tokens += token
                    case Left(message) => return new Left(message)
                }
            }
        }
        tokens += new TokenEOI()
        new Right(tokens.toArray)
    }

    def skipWhitespace() = {
        while (source(current).isWhitespace && current < length) {
            advance()
        }
    }

    def advance() = {
        if (current < length) {
            current += 1
        }
    }

    def nextToken: Either[String, Token] = {
        advance()
        if (source(start).isDigit || source(start) == '.') {
            lexNumber()
        } else if (source(start).isLetter) {
            lexIdentifier()
        } else {
            source(start) match {
                case '+' => new Right(new TokenPlus())
                case '-' => new Right(new TokenDash())
                case '*' => new Right(new TokenStar())
                case '/' => new Right(new TokenSlash())
                case '^' => new Right(new TokenCaret())
                case '=' => new Right(new TokenEquals())
                case '(' => new Right(new TokenLParen())
                case ')' => new Right(new TokenRParen())
                case _ => new Left(s"${source(start)} is an unrecognized token.")
            }
        }
    }

    def lexNumber(): Either[String, Token] = {
        while (current < length &&
            (source(current).isDigit || source(current) == '.')) {
            advance()
        }
        val lexeme = source.substring(start, current)
        try {
            val value = lexeme.toDouble
            new Right(new TokenNumber(value))
        } catch {
            case ex : Throwable => new Left(s"Error parsing number '$lexeme'")
        }
    }

    def lexIdentifier(): Either[String, Token] = {
        while (current < length && source(current).isLetterOrDigit) {
            advance()
        }
        val lexeme = source.substring(start, current)
        if (constants.contains(lexeme)) {
            new Right(new TokenConstant(lexeme))
        } else if (functions.contains(lexeme)) {
            new Right(new TokenFunction(lexeme))
        } else {
            new Right(new TokenVariable(lexeme))
        }
    }
}
