import net.sf.jasperreports.engine.{JRException, JasperCompileManager, JasperReport}
import net.sf.jasperreports.engine.util.{JRLoader, JRSaver}
import net.sf.jasperreports.engine.xml.JRXmlLoader

import java.io.{File, FilenameFilter}
import scala.util.{Failure, Success, Try}

object Main {

  implicit class PrintOption[T](option: Option[T]) {
    def orPrint(message: => String): Option[T] = {
      if (option.isEmpty) println(message)
      option
    }
  }
  implicit class PrintError[T](result: Try[T]) {
    def orPrint(message: Throwable => String): Option[T] = result match {
      case Success(value) => Some(value)
      case Failure(error) => println(message(error)); None
    }
  }

  object JrxmlFilter extends FilenameFilter {
    def accept(dir: File, name: String): Boolean = name.endsWith(".jrxml")
  }

  def filterValidFiles(files: Iterable[File], depth: Int): Iterable[String] = files.flatMap { file =>
    if (file.isFile) file.getAbsolutePath :: Nil
    else if (file.isDirectory && depth > 0) filterValidFiles(file.listFiles(JrxmlFilter), depth - 1)
    else if (file.isDirectory) Nil
    else { println(s"File ${file.getAbsolutePath} doesn't exist"); Nil }
  }

  def filterValid(paths: Iterable[String], depth: Int): Iterable[String] =
    filterValidFiles(paths.map(new File(_)), depth)

  def compile(validArgs: Iterable[String]): Unit = {
    for {
      jrxml <- validArgs
      outputPath <- Some(jrxml).collect { case s"$prefix.jrxml" => s"$prefix.jasper" }
        .orPrint(s"$jrxml doesn't end with .jrxml")
      input = JRLoader.getFileInputStream(jrxml)
      design <- Try { JRXmlLoader.load(input) }
        .orPrint(e => s"Error loading report: $jrxml - ${e.getMessage}")
      report <- Try { JasperCompileManager.compileReport(design) }
        .orPrint(e => s"Error compiling report: $jrxml - ${e.getMessage}")
    } yield Try { JRSaver.saveObject(report, outputPath) }
      .orPrint(e => s"Error saving compiled report: $jrxml - ${e.getMessage}")
      .map { _ => println(s"Compiled $jrxml successfully") }
  }

  def main(args: Array[String]): Unit = if (args.nonEmpty) {
    val validArgs =
      if (args.head == "-r") filterValid(args.tail, Int.MaxValue)
      else filterValid(args, 1)
    compile(validArgs)
  } else println("Empty arguments")

}