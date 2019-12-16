package fr.istic.cal.interpreter.tests

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

import fr.istic.cal.interpreter.Cons
import fr.istic.cal.interpreter.ConsValue
import fr.istic.cal.interpreter.Cst
import fr.istic.cal.interpreter.CstValue
import fr.istic.cal.interpreter.Eq
import fr.istic.cal.interpreter.For
import fr.istic.cal.interpreter.Hd
import fr.istic.cal.interpreter.Interpreter.Memory
import fr.istic.cal.interpreter.Interpreter.interpreterCommand
import fr.istic.cal.interpreter.NlValue
import fr.istic.cal.interpreter.Repeat
import fr.istic.cal.interpreter.Set
import fr.istic.cal.interpreter.SetMult
import fr.istic.cal.interpreter.Tl
import fr.istic.cal.interpreter.Var
import fr.istic.cal.interpreter.VarExp
import fr.istic.cal.interpreter.Switch
import fr.istic.cal.interpreter.Case
import fr.istic.cal.interpreter.VarHd
import fr.istic.cal.interpreter.VarTl

class TestsCustom {

  @Test
  def Test_Repeat_1 = {
    val memory: Memory = List((Var("Y"), ConsValue(NlValue, ConsValue(NlValue, ConsValue(CstValue("z"), NlValue)))), (Var("Z"), NlValue))
    val expected = List((Var("Y"), ConsValue(CstValue("z"), NlValue)), (Var("Z"), NlValue));
    val result = interpreterCommand(Repeat(Eq(Hd(VarExp("Y")), Cst("z")), List(Set(Var("Y"), Tl(VarExp("Y"))))), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_Repeat_2 = {
    val memory: Memory = List((Var("Y"), ConsValue(NlValue, ConsValue(NlValue, CstValue("5")))), (Var("Z"), NlValue))
    val expected = List((Var("Y"), ConsValue(NlValue, CstValue("5"))), (Var("Z"), NlValue));
    val result = interpreterCommand(Repeat(Eq(Tl(VarExp("Y")), Cst("5")), List(Set(Var("Y"), Tl(VarExp("Y"))))), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_SetHd_1 = {
    val memory: Memory = List((Var("X"), ConsValue(NlValue, CstValue("5"))), (Var("Y"), CstValue("4")));
    val expected = List((Var("X"), ConsValue(CstValue("4"), CstValue("5"))), (Var("Y"), CstValue("4")));
    val result = interpreterCommand(Set(VarHd("X"), VarExp("Y")), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_SetHd_2 = {
    val memory: Memory = List((Var("X"), ConsValue(ConsValue(CstValue("3"), CstValue("2")), CstValue("5"))), (Var("Y"), CstValue("4")));
    val expected = List((Var("X"), ConsValue(CstValue("4"), CstValue("5"))), (Var("Y"), CstValue("4")));
    val result = interpreterCommand(Set(VarHd("X"), VarExp("Y")), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_SetHd_3 = {
    val memory: Memory = List((Var("X"), CstValue("2")), (Var("Y"), CstValue("4")));
    val expected = List((Var("X"), NlValue), (Var("Y"), CstValue("4")));
    val result = interpreterCommand(Set(VarHd("X"), VarExp("Y")), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_SetHd_4 = {
    val memory: Memory = List((Var("Y"), CstValue("4")));
    val expected = List((Var("X"), ConsValue(CstValue("4"), NlValue)), (Var("Y"), CstValue("4")));
    val result = interpreterCommand(Set(VarHd("X"), VarExp("Y")), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_SetTl_1 = {
    val memory: Memory = List((Var("X"), ConsValue(CstValue("5"), NlValue)), (Var("Y"), CstValue("4")));
    val expected = List((Var("X"), ConsValue(CstValue("5"), CstValue("4"))), (Var("Y"), CstValue("4")));
    val result = interpreterCommand(Set(VarTl("X"), VarExp("Y")), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_SetTl_2 = {
    val memory: Memory = List((Var("X"), ConsValue(CstValue("5"), ConsValue(CstValue("3"), CstValue("2")))), (Var("Y"), CstValue("4")));
    val expected = List((Var("X"), ConsValue(CstValue("5"), CstValue("4"))), (Var("Y"), CstValue("4")));
    val result = interpreterCommand(Set(VarTl("X"), VarExp("Y")), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_SetTl_3 = {
    val memory: Memory = List((Var("X"), CstValue("2")), (Var("Y"), CstValue("4")));
    val expected = List((Var("X"), NlValue), (Var("Y"), CstValue("4")));
    val result = interpreterCommand(Set(VarTl("X"), VarExp("Y")), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_SetTl_4 = {
    val memory: Memory = List((Var("Y"), CstValue("4")));
    val expected = List((Var("X"), ConsValue(NlValue, CstValue("4"))), (Var("Y"), CstValue("4")));
    val result = interpreterCommand(Set(VarTl("X"), VarExp("Y")), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_For_With_Modify_Iterator = {
    val memory: Memory = List((Var("Y"), ConsValue(CstValue("y"), CstValue("y"))), (Var("Z"), NlValue))
    val result = interpreterCommand(For(VarExp("Y"), List(Set(Var("Y"), Cons(VarExp("Y"), Cst("a"))))), memory);
    assertTrue(result.contains((Var("Y"), ConsValue(ConsValue(ConsValue(CstValue("y"), CstValue("y")), CstValue("a")), CstValue("a")))));
  }

  @Test
  def Test_SetMult_1 = {
    val memory: Memory = List((Var("V"), CstValue("1")), (Var("W"), CstValue("2")), (Var("X"), CstValue("3")), (Var("Y"), CstValue("4")));
    val expected = List((Var("V"), CstValue("3")), (Var("W"), CstValue("4")), (Var("X"), CstValue("3")), (Var("Y"), CstValue("4")));
    val result = interpreterCommand(SetMult(List(Var("V"), Var("W")), List(VarExp("X"), VarExp("Y"))), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_SetMult_2 = {
    val memory: Memory = List((Var("V"), CstValue("1")), (Var("X"), CstValue("3")));
    val expected = List((Var("V"), CstValue("3")), (Var("X"), CstValue("1")));
    val result = interpreterCommand(SetMult(List(Var("V"), Var("X")), List(VarExp("X"), VarExp("V"))), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_SetMult_3 = {
    val memory: Memory = List((Var("V"), CstValue("1")), (Var("W"), CstValue("2")), (Var("X"), CstValue("3")), (Var("Y"), CstValue("4")));
    try {
      val result = interpreterCommand(SetMult(List(Var("V")), List(VarExp("W"), VarExp("X"), VarExp("Y"))), memory);
      assertTrue(false)
    } catch {
      case e: Exception => assertTrue(true);
    }
  }

  @Test
  def Test_SetMult_4 = {
    val memory: Memory = List((Var("V"), ConsValue(NlValue, NlValue)), (Var("W"), ConsValue(CstValue("1"), NlValue)), (Var("X"), CstValue("3")), (Var("Y"), CstValue("4")));
    val expected: Memory = List((Var("V"), ConsValue(CstValue("3"), NlValue)), (Var("W"), ConsValue(CstValue("4"), NlValue)), (Var("X"), CstValue("3")), (Var("Y"), CstValue("4")));
    val result = interpreterCommand(SetMult(List(VarHd("V"), VarHd("W")), List(VarExp("X"), VarExp("Y"))), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_SetMult_5 = {
    val memory: Memory = List((Var("V"), ConsValue(NlValue, NlValue)), (Var("W"), ConsValue(CstValue("1"), NlValue)), (Var("X"), CstValue("3")), (Var("Y"), CstValue("4")));
    val expected: Memory = List((Var("V"), ConsValue(NlValue, CstValue("3"))), (Var("W"), ConsValue(CstValue("1"), CstValue("4"))), (Var("X"), CstValue("3")), (Var("Y"), CstValue("4")));
    val result = interpreterCommand(SetMult(List(VarTl("V"), VarTl("W")), List(VarExp("X"), VarExp("Y"))), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_SetMult_6 = {
    val memory: Memory = List((Var("V"), ConsValue(NlValue, NlValue)), (Var("W"), ConsValue(CstValue("1"), NlValue)), (Var("X"), CstValue("3")), (Var("Y"), CstValue("4")));
    val expected: Memory = List((Var("V"), ConsValue(CstValue("3"), NlValue)), (Var("W"), ConsValue(CstValue("1"), CstValue("3"))), (Var("X"), ConsValue(CstValue("1"), NlValue)), (Var("Y"), CstValue("4")));
    val result = interpreterCommand(SetMult(List(VarHd("V"), Var("X"), VarTl("W")), List(VarExp("X"), VarExp("W"), VarExp("X"))), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_SetMult_7 = {
    val memory: Memory = List((Var("V"), ConsValue(NlValue, NlValue)), (Var("W"), ConsValue(CstValue("1"), NlValue)), (Var("X"), CstValue("3")), (Var("Y"), CstValue("4")));
    try {
      val expected: Memory = List((Var("V"), ConsValue(CstValue("3"), NlValue)), (Var("W"), ConsValue(CstValue("1"), CstValue("3"))), (Var("X"), ConsValue(CstValue("1"), NlValue)), (Var("Y"), CstValue("4")));
      val result = interpreterCommand(SetMult(List(VarHd("V"), Var("V"), VarTl("W")), List(VarExp("X"), VarExp("W"), VarExp("X"))), memory);
    } catch {
      case e: Exception => assertTrue(true);
    }
  }

  @Test
  def Test_SetMult_8 = {
    val memory: Memory = List((Var("V"), ConsValue(NlValue, NlValue)), (Var("W"), ConsValue(CstValue("1"), NlValue)), (Var("X"), CstValue("3")), (Var("Y"), CstValue("4")));
    try {
      val expected: Memory = List((Var("V"), ConsValue(CstValue("3"), NlValue)), (Var("W"), ConsValue(CstValue("1"), CstValue("3"))), (Var("X"), ConsValue(CstValue("1"), NlValue)), (Var("Y"), CstValue("4")));
      val result = interpreterCommand(SetMult(List(Var("V"), VarTl("V"), VarTl("W")), List(VarExp("X"), VarExp("W"), VarExp("X"))), memory);
    } catch {
      case e: Exception => assertTrue(true);
    }
  }

  @Test
  def Test_Switch_1 = {
    val memory: Memory = List((Var("X"), CstValue("1")), (Var("Y"), CstValue("1")), (Var("Z"), CstValue("2")));
    val expected: Memory = List((Var("X"), CstValue("1")), (Var("Y"), CstValue("3")), (Var("Z"), CstValue("2")));
    val result = interpreterCommand(Switch(VarExp("X"), List(Case(VarExp("Y"), List(Set(Var("Y"), Cst("3")))), Case(VarExp("Z"), List(Set(Var("Z"), Cst("3")))))), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_Switch_2 = {
    val memory: Memory = List((Var("X"), CstValue("1")), (Var("Y"), CstValue("1")), (Var("Z"), CstValue("1")));
    val expected: Memory = List((Var("X"), CstValue("1")), (Var("Y"), CstValue("3")), (Var("Z"), CstValue("1")));
    val result = interpreterCommand(Switch(VarExp("X"), List(Case(VarExp("Y"), List(Set(Var("Y"), Cst("3")))), Case(VarExp("Z"), List(Set(Var("Z"), Cst("3")))))), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

  @Test
  def Test_Switch_3 = {
    val memory: Memory = List((Var("X"), CstValue("3")), (Var("Y"), CstValue("1")), (Var("Z"), CstValue("1")));
    val expected: Memory = List((Var("X"), CstValue("3")), (Var("Y"), CstValue("1")), (Var("Z"), CstValue("1")));
    val result = interpreterCommand(Switch(VarExp("X"), List(Case(VarExp("Y"), List(Set(Var("Y"), Cst("3")))), Case(VarExp("Z"), List(Set(Var("Z"), Cst("3")))))), memory);
    assertTrue(expected.forall(a => result.contains(a)) && result.forall(a => expected.contains(a)))
  }

}