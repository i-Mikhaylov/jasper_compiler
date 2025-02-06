import net.sf.jasperreports.engine.{JRException, JasperCompileManager, JasperReport}
import net.sf.jasperreports.engine.util.{JRLoader, JRSaver}
import net.sf.jasperreports.engine.xml.JRXmlLoader

import java.io.File
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

  def filterValid(paths: Iterable[String]): Iterable[String] = paths.flatMap { path =>
    val file = new File(path)
    if (file.isFile) path :: Nil
    else if (file.isDirectory) filterValid(file.list)
    else { println(s"File $path doesn't exist"); Nil }
  }

  def run(args: Iterable[String]): Unit = {
    for {
      jrxml <- filterValid(args)
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

  def main(args: Array[String]): Unit = run(args ++ List("gg"))

}