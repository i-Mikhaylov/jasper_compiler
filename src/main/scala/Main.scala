import Printer.{printError, printInfo, printSuccess, printWarn}
import net.sf.jasperreports.engine.JasperCompileManager
import net.sf.jasperreports.engine.util.JRSaver

import java.io.{ByteArrayInputStream, File}
import java.nio.file.Files
import scala.io.AnsiColor
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}


object Printer {
  private def coloredPrint(message: String, ansiColor: AnsiColor => String)(implicit file: File): Unit =
    println(file.getAbsolutePath + ": " + ansiColor(Console) + message + Console.RESET)
  def printSuccess(message: String)(implicit file: File): Unit = coloredPrint(message, _.GREEN)
  def printWarn   (message: String)(implicit file: File): Unit = coloredPrint(message, _.YELLOW)
  def printInfo   (message: String)(implicit file: File): Unit = coloredPrint(message, _.YELLOW)
  def printError  (message: String)(implicit file: File): Unit = coloredPrint(message, _.RED)
}

implicit class PrintError[T](result: Try[T]):
  def orPrint(message: String)(implicit file: File): Option[T] = result match {
    case Success(value) => Some(value)
    case Failure(error) => printError(message + " - " + error.getMessage); None
  }


object Rewriter:
  private var instances: List[Rewriter] = Nil
  def rewrite(file: File): String =
    val orig = new String(Files.readAllBytes(file.toPath))
    instances.foldLeft(orig) { (data, rewriter) => rewriter.rewrite(data)(file) getOrElse data }

abstract class Rewriter:
  val _: Unit = { Rewriter.instances = this :: Rewriter.instances }
  def rewrite(orig: String)(implicit file: File): Option[String]

val JsonSourceRewriter: Rewriter = new Rewriter():
  val jsonLocal = "net.sf.jasperreports.json.data.JsonDataSource"
  val jsonServer = "net.sf.jasperreports.engine.data.JsonDataSource"
  def rewrite(orig: String)(implicit file: File): Option[String] =
    val replaced = orig.replace(jsonLocal, jsonServer)
    Option.when(replaced != orig) {
      printInfo(s"Replaced \"$jsonLocal\" -> \"$jsonServer\"")
      replaced
    }

val SubreportDirectoryRewriter: Rewriter = new Rewriter():
  val regex: Regex = "<subreportExpression><!\\[CDATA\\[\"(.*)\"]]></subreportExpression>".r
  def replace(implicit file: File): Regex.Match => String = matched =>
    val prefix = matched.source.subSequence(matched.start(0), matched.start(1)).toString
    val value  = matched.source.subSequence(matched.start(1), matched.end(1)).toString
    val suffix = matched.source.subSequence(matched.end(1), matched.end(0)).toString
    if (value startsWith "./reports")
      printWarn(s"Subreport expression is already contains subdirectory - \"$value\"")
      prefix + value + suffix
    else
      val valueReplaced = "./reports/" + value
      printInfo(s"Replaced \"$value\" -> \"$valueReplaced\"")
      prefix + valueReplaced + suffix
  def rewrite(orig: String)(implicit file: File): Option[String] =
    val rewrited = regex.replaceAllIn(orig, replace)
    Option.when(rewrited != orig)(rewrited)


object Main:

  def compile(source: File, destination: File): Unit =
    implicit val printSource: File = source
    val convertedXml = Rewriter.rewrite(source).getBytes
    val input = new ByteArrayInputStream(convertedXml)

    for {
      outputName <- Some(source.getName).collect { case s"$prefix.jrxml" => s"$prefix.jasper" }
      report <- Try { JasperCompileManager.compileReport(input) }.orPrint(s"Error compiling report")
    } yield Try { JRSaver.saveObject(report, new File(destination, outputName)) }
      .orPrint("Error saving compiled report")
      .map { _ => Files.write(new File(destination, source.getName).toPath, convertedXml) }
      .map { _ => printSuccess("Compiled successfully") }

  def main(args: Array[String]): Unit = args.map(new File(_)) match {
    case Array(source, destination) =>
      if (source.isFile) compile(source, destination)
      else if (source.isDirectory) source.listFiles().foreach { f => if (f.isFile) compile(f, destination) }
      else printError("Illegal path")(source)
    case _ => throw new IllegalArgumentException(s"Invalid argument number (${args.length}), but 2 expected")
  }
