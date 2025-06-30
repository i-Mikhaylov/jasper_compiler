import Printer.{printError, printInfo, printSuccess, printWarn}
import net.sf.jasperreports.engine.JasperCompileManager
import net.sf.jasperreports.engine.util.JRSaver

import java.io.{ByteArrayInputStream, File}
import java.nio.file.Files
import scala.io.AnsiColor
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}


object Printer:
  private def coloredPrint(message: String, ansiColor: AnsiColor => String)(implicit file: File): Unit =
    println(file.getName + ": " + ansiColor(Console) + message + Console.RESET)
  def printSuccess(message: String)(implicit file: File): Unit = coloredPrint(message, _.GREEN)
  def printWarn   (message: String)(implicit file: File): Unit = coloredPrint(message, _.YELLOW)
  def printInfo   (message: String)(implicit file: File): Unit = coloredPrint(message, _.YELLOW)
  def printError  (message: String)(implicit file: File): Unit = coloredPrint(message, _.RED)

implicit class PrintError[T](result: Try[T]):
  def orPrint(message: String)(implicit file: File): Option[T] = result match
    case Success(value) => Some(value)
    case Failure(error) => printError(message + " - " + error.getMessage); None


object Rewriter:
  private var instances: List[Rewriter] = Nil
  def rewrite(file: File): String =
    val orig = new String(Files.readAllBytes(file.toPath))
    instances.foldLeft(orig) { (data, rewriter) => rewriter.rewrite(data)(file) getOrElse data }

abstract class Rewriter:
  Rewriter.instances = this :: Rewriter.instances
  def rewrite(orig: String)(implicit file: File): Option[String]
abstract class RegexRewriter extends Rewriter:
  def regex: Regex
  def replace(implicit file: File): Regex.Match => String
  def rewrite(orig: String)(implicit file: File): Option[String] =
    val rewritten = regex.replaceAllIn(orig, replace)
    Option.when(rewritten != orig)(rewritten)

val JsonSourceRewriter: Rewriter = new Rewriter():
  val jsonLocal = "net.sf.jasperreports.json.data.JsonDataSource"
  val jsonServer = "net.sf.jasperreports.engine.data.JsonDataSource"
  def rewrite(orig: String)(implicit file: File): Option[String] =
    val rewritten = orig.replace(jsonLocal, jsonServer)
    Option.when(rewritten != orig) {
      printInfo(s"Replaced \"$jsonLocal\" -> \"$jsonServer\"")
      rewritten
    }

val SubreportDirectoryRewriter: Rewriter = new RegexRewriter():
  val regex: Regex = "<subreportExpression><!\\[CDATA\\[\"(.*)\"]]></subreportExpression>".r
  def replace(implicit file: File): Regex.Match => String = matched =>
    val prefix = matched.source.subSequence(matched.start(0), matched.start(1)).toString
    val value  = matched.source.subSequence(matched.start(1), matched.end(1)).toString
    val suffix = matched.source.subSequence(matched.end(1), matched.end(0)).toString
    if (value.startsWith("./reports"))
      printWarn(s"Subreport expression is already contains subdirectory - \"$value\"")
      prefix + value + suffix
    else
      printInfo(s"Replaced path for subreport \"$value\"")
      s"$prefix./reports/$value$suffix"

val ResourceDirectoryRewriter: Rewriter = new RegexRewriter():
  val regex: Regex = "\"/home/ivan/c4repo/cto-jasper-server/resources/(.*)\"".r
  def replace(implicit file: File): Regex.Match => String = matched =>
    val value = matched.group(1)
    printInfo(s"Replaced path for resource \"$value\"")
    s"\"/resources/$value\""


object Main:

  private var upToDatePaths: List[String] = Nil

  def compile(source: File, destinationDir: File): Unit =
    implicit val printSource: File = source
    lazy val convertedXml = Rewriter.rewrite(source).getBytes
    lazy val input = new ByteArrayInputStream(convertedXml)
    for
      destination <- Some(source.getName).collect { case s"$prefix.jrxml" => new File(destinationDir, s"$prefix.jasper") }
      upToDate = source.lastModified < destination.lastModified
      _ = if (upToDate) upToDatePaths = source.getName :: upToDatePaths
      if !upToDate
      report <- Try { JasperCompileManager.compileReport(input) }.orPrint(s"Error compiling report")
      success <- Try { JRSaver.saveObject(report, destination) }.orPrint("Error saving compiled report")
    yield
      Files.write(new File(destinationDir, source.getName).toPath, convertedXml)
      printSuccess("Compiled successfully")

  def main(args: Array[String]): Unit =
    args.map(new File(_)) match
      case Array(source, destination) =>
        if (source.isFile) compile(source, destination)
        else if (source.isDirectory) source.listFiles().foreach { f => if (f.isFile) compile(f, destination) }
        else printError("Illegal path")(source)
      case _ => throw new IllegalArgumentException(s"Invalid argument number (${args.length}), but 2 expected")
    if (upToDatePaths.nonEmpty) println("Up to date: " + upToDatePaths.sorted.mkString(", "))


//run "sbt package" to save compiled jar