package fr.istic.cal.interpreter.tests

import org.junit.Test
import org.junit.Assert._

import fr.istic.cal.interpreter.Interpreter._
import fr.istic.cal.interpreter._

class TestsExpressions {

  val memory: Memory = List((Var("X"), CstValue("xxx")), (Var("Y"), CstValue("yyy")), (Var("Z"), NlValue))

  @Test
  def Test_interpreterExpr_Nl(): Unit = {
    assertEquals(
      NlValue,
      interpreterExpr(Nl, memory))
  }

  @Test
  def Test_interpreterExpr_Cst(): Unit = {
    assertEquals(
      CstValue("xxx"),
      interpreterExpr(Cst("xxx"), memory))
  }

  @Test
  def Test_interpreterExprVar_OK(): Unit = {
    assertEquals(
      CstValue("xxx"),
      interpreterExpr(VarExp("X"), memory))
  }

  @Test
  def Test_interpreterExprVar_NOK(): Unit = {
    assertEquals(
      NlValue,
      interpreterExpr(VarExp("W"), memory))
  }

  @Test
  def Test_interpreterExpr_Cons(): Unit = {
    assertEquals(
      ConsValue(CstValue("xxx"), CstValue("yyy")),
      interpreterExpr(Cons(VarExp("X"), VarExp("Y")), memory))
  }

  @Test
  def Test_interpreterExpr_Hd_OK1(): Unit = {
    assertEquals(
      CstValue("xxx"),
      interpreterExpr(Hd(Cons(VarExp("X"), VarExp("Y"))), memory))
  }

  @Test
  def Test_interpreterExpr_Hd_OK2(): Unit = {
    val e: Expression = Cons(VarExp("X"), VarExp("Y"))
    assertEquals(
      CstValue("xxx"),
      interpreterExpr(Hd(e), memory))
  }

  @Test
  def Test_interpreterExpr_Hd_NOK(): Unit = {
    assertEquals(
      NlValue,
      interpreterExpr(Hd(VarExp("Z")), memory))
  }

  @Test
  def Test_interpreterExpr_Tl_OK1(): Unit = {
    assertEquals(
      CstValue("yyy"),
      interpreterExpr(Tl(Cons(VarExp("X"), VarExp("Y"))), memory))
  }

  @Test
  def Test_interpreterExpr_Tl_OK2(): Unit = {
    val e: Expression = Cons(VarExp("X"), VarExp("Y"))
    assertEquals(
      CstValue("yyy"),
      interpreterExpr(Tl(e), memory))
  }

  @Test
  def Test_interpreterExpr_Tl_NOK(): Unit = {
    assertEquals(
      NlValue,
      interpreterExpr(Tl(VarExp("Z")), memory))
  }

  @Test
  def Test_interpreterExpr_Eq_true(): Unit = {
    val e: Expression = Cons(VarExp("X"), VarExp("Y"))
    assertNotEquals(
      NlValue,
      interpreterExpr(Eq(VarExp("X"), Hd(e)), memory))
  }

  @Test
  def Test_interpreterExpr_Eq_false(): Unit = {
    val e: Expression = Cons(VarExp("X"), VarExp("Y"))
    assertEquals(
      NlValue,
      interpreterExpr(Eq(VarExp("Y"), Hd(e)), memory))
  }

  @Test
  def Test_interpreterExpr_Expr(): Unit = {
    val e: Expression = Cons(Hd(Cons(VarExp("X"), Cst("yyy"))), Tl(VarExp("W")))
    assertEquals(
      ConsValue(CstValue("xxx"), NlValue),
      interpreterExpr(e, memory))
  }

  @Test
  def Test_valueToExpression_Nl(): Unit = {
    assertEquals(
      Nl,
      valueToExpression(NlValue))
  }

  @Test
  def Test_valueToExpression_Cst(): Unit = {
    assertEquals(
      Cst("aaa"),
      valueToExpression(CstValue("aaa")))
  }

  @Test
  def Test_valueToExpression_Cons(): Unit = {
    assertEquals(
      Cons(Cst("aaa"), Nl),
      valueToExpression(ConsValue(CstValue("aaa"), NlValue)))
  }

  @Test
  def Test_valueToExpression_value(): Unit = {
    assertEquals(
      Cons(Cons(Nl, Cst("aaa")), Cons(Nl, Nl)),
      valueToExpression(ConsValue(ConsValue(NlValue, CstValue("aaa")), ConsValue(NlValue, NlValue))))
  }
}

