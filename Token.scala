sealed class Token {}
case class TokenPlus() extends Token {
    override def toString(): String = "+"
}
case class TokenDash() extends Token {
    override def toString(): String = "-"
}

case class TokenStar() extends Token {
    override def toString(): String = "*"
}
case class TokenSlash() extends Token {
    override def toString(): String = "/"
}
case class TokenCaret() extends Token {
    override def toString(): String = "^"
}
case class TokenEquals() extends Token {
    override def toString(): String = "="
}
case class TokenLParen() extends Token {
    override def toString(): String = "("
}
case class TokenRParen() extends Token {
    override def toString(): String = ")"
}
case class TokenNumber(val value: Double) extends Token {
    override def toString(): String = value.toString
}
case class TokenVariable(val name: String) extends Token {
    override def toString(): String = name
}
case class TokenFunction(val name: String) extends Token {
    override def toString(): String = name
}
case class TokenConstant(val name: String) extends Token {
    override def toString(): String = name
}
case class TokenEOI() extends Token {
    override def toString(): String = "♣"
}
