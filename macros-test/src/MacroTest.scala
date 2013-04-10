import org.junit.Test
import org.junit.Assert._

class MacroTest {

  @Test def testTom() {
    assertEquals("Tom says hi", Macro.caseStatement("/tom/hi"))
  }

  @Test def testDick() {
    assertEquals("Dick says hi", Macro.caseStatement("/dick/hi"))
  }

  @Test def testHarry() {
    assertEquals("Harry says hi", Macro.caseStatement("/harry/hi"))
  }

  @Test def testError() {
    assertEquals("Error", Macro.caseStatement("/fred"))
  }
  
  @Test def testMacroTom() {
    assertEquals("Tom says hi", MacroTest.macroCase("/tom/hi"))
  }

  @Test def testMacroDick() {
    assertEquals("Dick says hi", MacroTest.macroCase("/dick/hi"))
  }

  @Test def testMacroHarry() {
    assertEquals("Harry says hi", MacroTest.macroCase("/harry/hi"))
  }

  @Test def testMacroError() {
    assertEquals("Error", MacroTest.macroCase("/fred"))
  }
  
}
object MacroTest {
  def macroExpanded(path : String) = Macro.macroCase("GET", path)
  val macroCase = macroExpanded _
}