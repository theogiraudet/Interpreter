package fr.istic.cal.interpreter

import scala.util.parsing.combinator.RegexParsers

object WhileParser extends RegexParsers {

  def constanteetnil = """[a-z]+[0-9]*""".r ^^ {
    case "nil" => Nl
    case x     => Cst(x.toString)
  }
  def variableexp = """[A-Z]+[0-9]*""".r ^^ { case x => VarExp(x.toString) }

  def expressioncons: Parser[Expression] = "(" ~ "cons" ~ expression ~ expression ~ ")" ^^ { case "(" ~ cons ~ e1 ~ e2 ~ ")" => Cons(e1, e2) }
  def expressionhd: Parser[Expression] = "(" ~ "hd" ~ expression ~ ")" ^^ { case "(" ~ "hd" ~ e ~ ")" => Hd(e) }
  def expressiontl: Parser[Expression] = "(" ~ "tl" ~ expression ~ ")" ^^ { case "(" ~ "tl" ~ e ~ ")" => Tl(e) }

  def expression: Parser[Expression] = expressionsansegal ~ opt("=?" ~ expression) ^^ {
    case e1 ~ None         => e1
    case e1 ~ Some(_ ~ e2) => Eq(e1, e2)
  } | "(" ~ expression ~ ")" ^^ { case _ ~ e ~ _ => e }

  def nop: Parser[Command] = "nop" ^^ { case _ => Nop }
  def twhile: Parser[Command] = "while" ~ expression ~ "do" ~ commandlist ~ "od" ^^ { case _ ~ e ~ _ ~ l ~ _ => While(e, l) }
  def tfor: Parser[Command] = "for" ~ expression ~ "do" ~ commandlist ~ "od" ^^ { case _ ~ e ~ _ ~ l ~ _ => For(e, l) }
  def tif: Parser[Command] = "if" ~ expression ~ "then" ~ commandlist ~ opt("else" ~ commandlist) ~ "fi" ^^ {
    case _ ~ e ~ _ ~ l1 ~ None ~ _         => If(e, l1, List(Nop))
    case _ ~ e ~ _ ~ l1 ~ Some(_ ~ l2) ~ _ => If(e, l1, l2)
  }
  def commandlist: Parser[List[Command]] = command ~ opt(";" ~ commandlist) ^^ {
    case c ~ None        => List(c)
    case c ~ Some(_ ~ l) => c :: l
  }
  def variablelist: Parser[List[Variable]] = variable ~ opt("," ~ variablelist) ^^ {
    case v ~ None        => List(v)
    case v ~ Some(_ ~ l) => v :: l
  }

  def program: Parser[Program] = "read" ~ variablelist ~ "%" ~ commandlist ~ "%" ~ "write" ~ variablelist ^^ { case _ ~ in ~ _ ~ c ~ _ ~ _ ~ out => Progr(in, c, out) }

  //Structures modifiées
  def simplevariable: Parser[Variable] = """[A-Z]+[0-9]*""".r ^^ { case x => Var(x.toString) }
  def variable: Parser[Variable] = simplevariable | hdvariable | tlvariable
  def expressionsansegal: Parser[Expression] = constanteetnil | variableexp | expressioncons | expressionhd | expressiontl
  def command: Parser[Command] = nop | assign | twhile | tfor | tuntil | tif | switch
  //On pourrait seulement considérer Set comme prenant une liste mais cela casserait les tests fournis
  def assign: Parser[Command] = variablelist ~ ":=" ~ expressionlist ^^ { case v ~ _ ~ e => if(v.size == 1) Set(v.head, e.head) else SetMult(v, e) }
  
  //Structures ajoutées
  def hdvariable: Parser[Variable] = "(" ~> "hd" ~> """[A-Z]+[0-9]*""".r <~ ")" ^^ { case v => VarHd(v); }
  def tlvariable: Parser[Variable] = "(" ~> "tl" ~> """[A-Z]+[0-9]*""".r ~ ")" ^^ { case v ~ _ => VarTl(v); }
  
  def expressionlist: Parser[List[Expression]] =  expression ~ opt("," ~ expressionlist) ^^ {
    case v ~ None        => List(v)
    case v ~ Some(_ ~ l) => v :: l
  }
 
  def caseS: Parser[Case] = "case" ~ expression ~ ":" ~ commandlist ^^ { case _ ~ e ~ _ ~ cmdL => Case(e, cmdL); }
  
  def casesList: Parser[List[Case]] = caseS ~ opt(casesList) ^^ {
    case v ~ None        => List(v);
    case v ~ Some(l) => v :: l;
  }
  
  def switch: Parser[Command] = "switch" ~ expression ~ "in" ~ casesList ~ "ni" ^^ { case _ ~ e ~ _ ~ cases ~ _ => Switch(e, cases); }
  def tuntil: Parser[Command] = "repeat" ~ commandlist ~ "until" ~ expression ^^ { case _ ~ l ~ _ ~ e => Repeat(e, l); }

  def analyserexpression(s: String): Expression = {
    WhileParser.parseAll(expression, s) match {
      case Success(result, _) => result
      case Failure(msg, _)    => throw new Exception("FAILURE: " + msg)
      case Error(msg, _)      => throw new Exception("ERROR: " + msg)
    }
  }

  def analysercommand(s: String): Command = {
    WhileParser.parseAll(command, s) match {
      case Success(result, _) => result
      case Failure(msg, _)    => throw new Exception("FAILURE: " + msg)
      case Error(msg, _)      => throw new Exception("ERROR: " + msg)
    }
  }
  def analyserprogram(s: String): Program = {
    WhileParser.parseAll(program, s) match {
      case Success(result, _) => result
      case Failure(msg, _)    => throw new Exception("FAILURE: " + msg)
      case Error(msg, _)      => throw new Exception("ERROR: " + msg)
    }
  }

}

object TestSimpleParser extends App {
  println(WhileParser.analyserprogram("read X,  G,           K % \n Y := nil ; while X do Y := ( cons (hd X     ) Y); ( hd X ) := ( tl X) od % write Y, Z, T "))
  println(WhileParser.analyserprogram("read X,  G,           K % \n Y := nil ; switch X in case Y : X := X case Z : nop ni; ( hd X ), Y := ( tl X), ( hd X ) % write Y, Z, T "))
}