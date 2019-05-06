import scala.collection.mutable.ArrayBuffer
import scala.util.{Either, Left, Right}

class Parser(val tokens: Array[Token]) {
    val length = tokens.length
    var current = 0

    def peek(n: Int): Token = {
        if (current + n < length) {
            tokens(current + n)
        } else {
            tokens(length - 1)
        }
    }

    def advance(): Unit = {
        if (current < length) {
            current += 1
        }
    }

    def parse(): Either[String, ASTNode] = assignment

    def assignment(): Either[String, ASTNode] = {
        peek(0) match {
            case TokenVariable(varName) => {
                peek(1) match {
                    case TokenEquals() => {
                        advance
                        advance
                        expression match {
                            case Right(expr) => new Right(new ASTAssign(varName, expr))
                            case Left(message) => new Left(message)
                        }
                    }
                    case _ => expression
                }
            }
            case _ => expression
        }
    }

    def expression(): Either[String, ASTNode] = {
        val arguments: ArrayBuffer[ASTNode] = new ArrayBuffer()
        var loop = true
        factor match {
            case Right(f) => arguments += f
            case Left(message) => return new Left(message)
        }
        while (loop && (current < length)) {
            tokens(current) match {
                case TokenEOI() => loop = false
                case TokenRParen() => loop = false
                case TokenPlus() => {
                    advance
                    factor match {
                        case Right(f) => arguments += f
                        case Left(message) => return new Left(message)
                    }
                }
                case TokenDash() => {
                    advance
                    factor match {
                        case Right(f) => arguments += new ASTTimes(List(new ASTNumber(-1.0), f))
                        case Left(message) => return new Left(message)
                    }
                }
                case token => {
                    return new Left(s"Expected '+' or '-' not '${token.toString}'")
                }
            }
        }
        arguments.length match {
            case 0 => new Right(new ASTNumber(0.0))
            case 1 => new Right(arguments(0))
            case _ => new Right(new ASTPlus(arguments.toList))
        }
    }

    def factor(): Either[String, ASTNode] = {
        var arguments: ArrayBuffer[ASTNode] = new ArrayBuffer()
        var loop = true
        exponential match {
            case Right(exp) => arguments += exp
            case Left(message) => return new Left(message)
        }
        while (loop && (current < length)) {
            peek(0) match {
                case TokenEOI() => loop = false
                case TokenRParen() => loop = false
                case TokenPlus() => loop = false
                case TokenDash() => loop = false
                case TokenStar() => {
                    advance
                    exponential match {
                        case Right(exp) => arguments += exp
                        case Left(message) => return new Left(message)
                    }
                }
                case TokenSlash() => {
                    advance
                    exponential match {
                        case Right(exp) => arguments += new ASTPower(exp, new ASTNumber(-1.0))
                        case Left(message) => new Left(message)
                    }
                }
                case token => new Left(s"Expected '*' or '/' not '${token.toString}'")
            }
        }
        arguments.length match {
            case 0 => new Right(new ASTNumber(1.0))
            case 1 => new Right(arguments(0))
            case _ => new Right(new ASTTimes(arguments.toList))
        }
    }

    def exponential(): Either[String, ASTNode] = {
        var curToken: Token = peek(0)
        var negate = false
        if (curToken.isInstanceOf[TokenDash]) {
            advance
            negate = true
        }
        var result: ASTNode = null
        atom match {
            case Right(atm) => result = atm
            case Left(message) => return new Left(message)
        }
        curToken = peek(0)
        if (curToken.isInstanceOf[TokenCaret]) {
            advance
            exponential match {
                case Right(atm) => result = new ASTPower(result, atm)
                case Left(message) => new Left(message)
            }
        }
        if (negate) {
            result = new ASTTimes(List(new ASTNumber(-1.0), result))
        }
        return new Right(result)
    }

    def atom(): Either[String, ASTNode] = {
        val curToken = peek(0)
        advance
        curToken match {
            case TokenNumber(num) => new Right(new ASTNumber(num))
            case TokenVariable(name) => new Right(new ASTVariable(name))
            case TokenConstant(name) => new Right(new ASTConstant(name))
            case TokenFunction(name) => {
                atom match {
                    case Right(arg) => new Right(new ASTFunction(name, arg))
                    case Left(message) => return new Left(message)
                }
            }
            case TokenLParen() => {
                val exprResult = expression
                peek(0) match {
                    case TokenRParen() => {
                        advance
                        exprResult
                    }
                    case _ => new Left("Unbalanced parentheses")
                }
            }
            case _ => new Left(s"Expected a number, constant, or variable instead of ${curToken.toString}")
        }
    }
}
