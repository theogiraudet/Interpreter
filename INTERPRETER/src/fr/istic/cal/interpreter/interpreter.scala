package fr.istic.cal.interpreter

/**
 * définition d'une exception pour le cas des listes vides
 */
case object ExceptionListeVide extends Exception

/**
 * définition d'une exception pour le cas des listes de tailles différentes
 */
case object ExceptionListesDeLongueursDifferentes extends Exception

/**
 * définition d'une exception pour le cas de plusieurs affectations à une même variable dans une affectation parallèle
 */
case object DuplicateAffectationException extends Exception


object Interpreter {

  /**
   * UTILISATION D'UN ANALYSEUR SYNTAXIQUE POUR LE LANGAGE WHILE
   *
   * les 3 fonctions suivantes permettent de construire un arbre de syntaxe abstraite
   * respectivement pour une expression, une commande, un programme
   */

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une expression du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette expression
   */
  def readWhileExpression(s: String): Expression = { WhileParser.analyserexpression(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'une commande du langage WHILE
   * @return un arbre de syntaxe abstraite pour cette commande
   */
  def readWhileCommand(s: String): Command = { WhileParser.analysercommand(s) }

  /**
   * @param s : une chaine de caractères représentant la syntaxe concrète d'un programme du langage WHILE
   * @return un arbre de syntaxe abstraite pour ce programme
   */
  def readWhileProgram(s: String): Program = { WhileParser.analyserprogram(s) }

  /**
   * UN INTERPRETER POUR LE LANGAGE WHILE
   *
   */

  /**
   *  GESTION DE LA MEMOIRE DE L'INTERPRETEUR
   */

  /**
   *  définition d'un type Memory pour représenter une mémoire
   */
  type Memory = List[(Variable, Value)]

  /**
   * @param v : une variable
   * @param mem : une mémoire
   * @return la valeur de la variable v dans la mémoire mem,
   * la valeur par défaut si la variable v n'est pas présente dans la mémoire mem
   */
  def lookUp(v: Variable, mem: Memory): Value = {
    mem match {
      case Nil               => NlValue;
      case (vari, valu) :: y => if (vari == v) valu else lookUp(v, y);
    }
  }

  /**
   * @param v : une variable
   * @param e : une expression
   * @param mem : une mémoire
   * @return la mémoire augmentée de l'assignation [v->d] si v n'était pas présente dans la mémoire,
   * modifiée pour prendre en compte la nouvelle valeur de v sinon
   */
  def assign(v: Variable, e: Value, mem: Memory): Memory = {
    mem match {
      case Nil               => (v, e) :: Nil;
      case (vari, valu) :: y => if (vari == v) (v, e) :: y else (vari, valu) :: assign(v, e, y);
    }
  }

  /**
   *  TRAITEMENT DES EXPRESSIONS DU LANGAGE WHILE
   */

  /**
   * @param expression : un AST décrivant une expression du langage WHILE
   * @return la valeur de l'expression
   */
  def interpreterExpr(expression: Expression, mem: Memory): Value = {
    expression match {
      case Nl         => NlValue;
      case Cst(x)     => CstValue(x);
      case VarExp(x)  => lookUp(Var(x), mem);
      case Cons(x, y) => ConsValue(interpreterExpr(x, mem), interpreterExpr(y, mem));
      case Hd(x)      => splitConsValue(interpreterExpr(x, mem))._1;
      case Tl(x)      => splitConsValue(interpreterExpr(x, mem))._2;
      case Eq(x, y)   => if (interpreterExpr(x, mem) != interpreterExpr(y, mem)) NlValue else CstValue("true");
    }
  }

  /**
   * @param v : une valeur
   * @return un couple (Value, Value) contenant la tête et la queue de v si celle-ci en possède ou un couple de NlValue sinon. 
   */
  def splitConsValue(v: Value): (Value, Value) = {
    v match {
      case NlValue         => (NlValue, NlValue);
      case CstValue(x)     => (NlValue, NlValue);
      case ConsValue(x, y) => (x, y);
    }
  }

  /**
   * la fonction interpreterExpr ci-dessus calcule la valeur associée à une expression
   * il peut être utile de produire à l'inverse une expression associée à une valeur
   * la fonction valueToExpression ci-dessous construira l'expression la plus simple associée à une valeur
   *
   * @param value : une valeur du langage WHILE
   * @return l'AST décrivant une expression de cette valeur
   */
  def valueToExpression(value: Value): Expression = {
    value match {
      case NlValue         => Nl;
      case CstValue(x)     => Cst(x);
      case ConsValue(x, y) => Cons(valueToExpression(x), valueToExpression(y));
    }
  }

  /**
   *
   *  TRAITEMENT DES COMMANDES DU LANGAGE WHILE
   */

  /**
   * @param command : un AST décrivant une commande du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de command
   */
  def interpreterCommand(command: Command, memory: Memory): Memory = {
    command match {
      case Nop          => memory;
      case Set(x, y)    => affectValue(x, interpreterExpr(y, memory), memory);
      case If(x, y, z)  => if (interpreterExpr(x, memory) != NlValue) interpreterCommands(y, memory)
                           else interpreterCommands(z, memory);
      case While(x, y)  => if (interpreterExpr(x, memory) == NlValue) memory 
                           else interpreterCommand(command, interpreterCommands(y, memory));
      case For(x, y)    => if (interpreterExpr(x, memory) == NlValue) memory 
                           else interpreterCommand(For(Tl(valueToExpression(interpreterExpr(x, memory))), y), interpreterCommands(y, memory));
      case Repeat(x, y) => { val value = interpreterCommands(y, memory); 
                             if (interpreterExpr(x, value) != NlValue) value else interpreterCommand(Repeat(x, y), value); 
                           }
      case SetMult(x, y) => if (x.size != y.size) throw ExceptionListesDeLongueursDifferentes
                            else { 
                              val list = getVarName(x);
                              if(list.distinct.size != list.size) throw DuplicateAffectationException;
                              else {
                                val values = y.map(interpreterExpr(_, memory));
                                x.zip(values).foldLeft(memory)((mem, pair) => affectValue(pair._1, pair._2, mem));
                              }
                            }
      case Switch(e, x) => interpreterCase(e, x, memory);
    }
  }
  
  /**
   * @param l : une liste de variables
   * @return une liste de String contenant le nom de chacune des variables
   */
  def getVarName(l: List[Variable]): List[String] = {
    l match {
      case Nil => Nil;
      case Var(x) :: y => x :: getVarName(y);
      case VarHd(x) :: y => x :: getVarName(y);
      case VarTl(x) :: y => x :: getVarName(y);
    }
  }
  
  /**
   * @param v : une variable
   * @param value : une valeur à affecter
   * @param mem : une mémoire
   * @return la nouvelle mémoire après affectation de value à v en fonction de la nature de la variable
   */
  def affectValue(v: Variable, value: Value, mem: Memory): Memory = {
    v match {
      case Var(x) => assign(v, value, mem);
      case VarHd(x) => affectValue(Var(x), lookUp(Var(x), mem) match {
                        case CstValue(_) => NlValue;
                        case a           => ConsValue(value, interpreterExpr(Tl(valueToExpression(a)), mem));
                       }, mem);
      case VarTl(x) => affectValue(Var(x), lookUp(Var(x), mem) match {
                        case CstValue(_) => NlValue;
                        case a           => ConsValue(interpreterExpr(Hd(valueToExpression(a)), mem), value);
                       }, mem);
    }
  }

  /**
   * @param exp : une expression à comparer avec chaque expression de list
   * @param list : une liste de Case à évaluer
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation du Case dont l'expression correspond à exp ou la mémoire telle quelle si aucune correspondance
   */
  def interpreterCase(exp: Expression, list: List[Case], memory: Memory): Memory = {
    list match {
      case Nil    => memory
      case x :: y => if (interpreterExpr(Eq(exp, x.condition), memory) != NlValue) interpreterCommands(x.body, memory) 
                     else interpreterCase(exp, y, memory);
    }
  }

  /**
   * @param commands : une liste d'AST décrivant une liste de commandes du langage WHILE
   * @param memory : une mémoire
   * @return la mémoire après l'interprétation de la liste de commandes
   */
  def interpreterCommands(commands: List[Command], memory: Memory): Memory = {
    commands match {
      case Nil      => throw ExceptionListeVide;
      case x :: Nil => interpreterCommand(x, memory);
      case x :: y   => interpreterCommands(y, interpreterCommand(x, memory));
    }
  }

  /**
   *
   *  TRAITEMENT DES PROGRAMMES DU LANGAGE WHILE
   */

  /**
   * @param vars : une liste décrivant les variables d'entrée d'un programme du langage WHILE
   * @param vals : une liste de valeurs
   * @return une mémoire associant chaque valeur à la variable d'entrée correspondant
   */
  def interpreterMemorySet(vars: List[Variable], vals: List[Value]): Memory = {
    if (vars.size != vals.size)
      throw ExceptionListesDeLongueursDifferentes;
    else
      vars zip vals;
  }

  /**
   * @param vars : une liste décrivant les variables de sortie d'un programme du langage WHILE
   * @param memory : une mémoire
   * @return la liste des valeurs des variables de sortie
   */
  def interpreterMemoryGet(vars: List[Variable], memory: Memory): List[Value] = {
    val list = vars.map(lookUp(_, memory));
    if (list.isEmpty)
      throw ExceptionListeVide;
    else
      list;
  }

  /**
   * @param program : un AST décrivant un programme du langage WHILE
   * @param vals : une liste de valeurs
   * @return la liste des valeurs des variables de sortie
   */
  def interpreter(program: Program, vals: List[Value]): List[Value] = {
    program match {
      case Progr(x, y, z) => interpreterMemoryGet(z, interpreterCommands(y, interpreterMemorySet(x, vals)));
    }
  }

}