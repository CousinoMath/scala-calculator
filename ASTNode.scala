import scala.collection.mutable.Map;

sealed abstract class ASTNode {
    def evaluate(memory: Map[String, Double]): Double
}
case class ASTPlus(val args: List[ASTNode]) extends ASTNode {
    override def evaluate(memory: Map[String, Double]): Double = 
        args.map(arg => arg.evaluate(memory)).sum

    override def toString(): String = {
        val argsString = args.fold("")((accum, arg) => accum + " " + arg.toString)
        s"(+$argsString)"
    }
}

case class ASTTimes(val args: List[ASTNode]) extends ASTNode {
    override def evaluate(memory: Map[String, Double]): Double =
        args.map(arg => arg.evaluate(memory)).product

    override def toString(): String = {
        val argsString = args.fold("")((accum, arg) => accum + " " + arg.toString)
        s"(*$argsString)"
    }
}

case class ASTPower(val base: ASTNode, val exp: ASTNode) extends ASTNode {
    override def evaluate(memory: Map[String, Double]): Double =
        Math.pow(base.evaluate(memory), exp.evaluate(memory))

    override def toString(): String = s"(^ ${base.toString} ${exp.toString})"
}

case class ASTAssign(val name: String, val expr: ASTNode) extends ASTNode {
    override def evaluate(memory: Map[String, Double]): Double = {
        val keyValue: (String, Double) = (name, expr.evaluate(memory))
        memory += keyValue
        keyValue._2
    }

    override def toString(): String = s"(= $name ${expr.toString})"
}

case class ASTFunction(val name: String, val arg: ASTNode) extends ASTNode {
    override def evaluate(memory: Map[String, Double]): Double = {
        val argValue = arg.evaluate(memory)
        name match {
            case "acos" => Math.acos(argValue)
            case "asin" => Math.asin(argValue)
            case "atan" => Math.atan(argValue)
            case "cos" => Math.cos(argValue)
            case "exp" => Math.exp(argValue)
            case "log" => Math.log(argValue)
            case "sin" => Math.sin(argValue)
            case "tan" => Math.tan(argValue)
            case _ => Double.NaN
        }
    }

    override def toString(): String = s"($name ${arg.toString})"
}

case class ASTNumber(val value: Double) extends ASTNode {
    override def evaluate(memory: Map[String, Double]): Double =
        value

    override def toString(): String = value.toString
}

case class ASTConstant(val name: String) extends ASTNode {
    override def evaluate(memory: Map[String, Double]): Double =
        name match {
            case "pi" => Math.PI
            case "e" => Math.E
            case _ => Double.NaN
        }

    override def toString(): String = name
}

case class ASTVariable(val name: String) extends ASTNode {
    override def evaluate(memory: Map[String, Double]): Double =
        memory.get(name).getOrElse(Double.NaN)

    override def toString(): String = name
}
