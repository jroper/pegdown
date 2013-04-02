package org.pegdown

import java.io.{StringWriter, StringReader}
import org.specs2.mutable.Specification
import org.w3c.tidy.Tidy
import org.parboiled.common.FileUtils
import org.parboiled.support.ToStringFormatter
import org.parboiled.trees.GraphUtils
import ast.Node


abstract class AbstractPegDownSpec extends Specification {

  def test(testName: String)(implicit processor: PegDownProcessor) {
    implicit val serializer = new ToHtmlSerializer(new LinkRenderer)
    testWithSerializer(testName)
  }

  def test(testName: String, expectedOutput: String)(implicit processor: PegDownProcessor) {
    implicit val serializer = new ToHtmlSerializer(new LinkRenderer)
    testWithSerializer(testName, expectedOutput)
  }

  def testAST(testName: String)(implicit processor: PegDownProcessor) {
    val markdown = FileUtils.readAllCharsFromResource(testName + ".md")
    require(markdown != null, "Test '" + testName + "' not found")

    val expectedAst = FileUtils.readAllTextFromResource(testName + ".ast")
    require(expectedAst != null, "Expected AST for '" + testName + "' not found")

    val astRoot = processor.parseMarkdown(markdown)

    // check parse tree
    //assertEquals(printNodeTree(getProcessor().parser.parseToParsingResult(markdown)), "<parse tree>");

    normalize(GraphUtils.printTree(astRoot, new ToStringFormatter[Node]())) === normalize(expectedAst)
  }

  def testWithSerializer(testName: String)(implicit processor: PegDownProcessor,
      serializer: ToHtmlSerializer) {
    val expectedUntidy = FileUtils.readAllTextFromResource(testName + ".html")
    require(expectedUntidy != null, "Test '" + testName + "' not found")
    testWithSerializer(testName, tidy(expectedUntidy))
  }

  def testWithSerializer(testName: String, expectedOutput: String)(implicit processor: PegDownProcessor,
      serializer: ToHtmlSerializer) {
    val markdown = FileUtils.readAllCharsFromResource(testName + ".md")
    require(markdown != null, "Test '" + testName + "' not found")

    val astRoot = processor.parseMarkdown(markdown)
    val actualHtml = serializer.toHtml(astRoot)

    // debugging I: check the parse tree
    //assertEquals(printNodeTree(getProcessor().parser.parseToParsingResult(markdown)), "<parse tree>");

    // debugging II: check the AST
    // GraphUtils.printTree(astRoot, new ToStringFormatter[Node]) === ""

    // debugging III: check the actual (untidied) HTML
    // actualHtml === ""

    // tidy up html for fair equality test
    val tidyHtml = tidy(actualHtml)
    normalize(tidyHtml) === normalize(expectedOutput)
  }

  def tidy(html: String) = {
    val in = new StringReader(html)
    val out = new StringWriter
    val t = new Tidy
    t.setTabsize(4)
    t.setPrintBodyOnly(true)
    t.setShowWarnings(false)
    t.setQuiet(true)
    t.parse(in, out)
    out.toString
  }

  def normalize(string: String) = string.replace("\r\n", "\n").replace("\r", "\n")

}
