package fr.istic.cal.interpreter.tests

import org.junit.Test
import org.junit.Assert._

import fr.istic.cal.interpreter.Interpreter._
import fr.istic.cal.interpreter._

class TestsMemoire {
  val memory: Memory = List((Var("X"), CstValue("xxx")), (Var("Y"), CstValue("yyy")), (Var("Z"), NlValue))

  @Test
  def Test_lookUp_positive(): Unit = {
    assertEquals(
      CstValue("yyy"),
      lookUp(Var("Y"), memory))
  }

  @Test
  def Test_lookUp_default1(): Unit = {
    assertEquals(
      NlValue,
      lookUp(Var("X"), Nil))
  }

  @Test
  def Test_lookUp_default2(): Unit = {
    assertEquals(
      NlValue,
      lookUp(Var("W"), memory))
  }

  @Test
  def Test_assign_default1(): Unit = {
    val expected =  memory ::: List((Var("W"), CstValue("www")))
    val result = assign(Var("W"), CstValue("www"), memory)
    assertTrue( expected.forall(a => result.contains(a))&&result.forall(a => expected.contains(a)) )
  }
  
  @Test
  def Test_assign_default2(): Unit = {
    assertEquals(
      List((Var("W"), CstValue("www"))),
      assign(Var("W"), CstValue("www"), Nil))
  }
  
  @Test
  def Test_assign_positive(): Unit = {
    val expected =  List((Var("X"), CstValue("xxx")), (Var("Y"), CstValue("www")), (Var("Z"), NlValue))
    val result = assign(Var("Y"), CstValue("www"), memory)
    assertTrue( expected.forall(a => result.contains(a))&&result.forall(a => expected.contains(a)) )
  }
}

