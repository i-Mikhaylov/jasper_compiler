import net.sf.jasperreports.engine.JasperCompileManager
import net.sf.jasperreports.engine.util.JRSaver
import net.sf.jasperreports.engine.xml.JRXmlLoader

import java.io.{ByteArrayInputStream, File}
import java.nio.file.{FileSystemException, Files}
import scala.util.{Failure, Success, Try}


object Main {

  def print(file: File, message: String, ansiColor: String): Unit =
    println(file.getAbsolutePath + ": " + ansiColor + message + Console.RESET)

  implicit class PrintError[T](result: Try[T]) {
    def orPrint(file: File, message: String): Option[T] = result match {
      case Success(value) => Some(value)
      case Failure(error) => print(file, message + " - " + error.getMessage, Console.RED); None
    }
  }

  def readConvertJrxml(source: File): Array[Byte] = {
    val origXml = Files.readAllBytes(source.toPath)
    val str = new String(origXml)

    val jsonLocal = "net.sf.jasperreports.json.data.JsonDataSource"
    val jsonServer = "net.sf.jasperreports.engine.data.JsonDataSource"
    val jsonReplaced = str.replace(jsonLocal, jsonServer)
    if (jsonReplaced != str) print(source, s"Replaced \"$jsonLocal\" -> \"$jsonServer\"", Console.YELLOW)

    jsonReplaced.getBytes
  }

  def compile(source: File, destination: File): Unit = {
    val convertedXml = readConvertJrxml(source)
    val input = new ByteArrayInputStream(convertedXml)

    for {
      outputName <- Some(source.getName).collect { case s"$prefix.jrxml" => s"$prefix.jasper" }
      design <- Try { JRXmlLoader.load(input) }.orPrint(source, "Error loading report - ")
      report <- Try { JasperCompileManager.compileReport(design) }.orPrint(source, s"Error compiling report - ")
    } yield Try { JRSaver.saveObject(report, new File(destination, outputName)) }
      .orPrint(source, "Error saving compiled report - ")
      .map { _ => Files.write(new File(destination, source.getName).toPath, convertedXml) }
      .map { _ => print(source, "Compiled successfully", Console.GREEN) }
  }

  def main(args: Array[String]): Unit = args.map(new File(_)) match {
    case Array(source, destination) =>
      if (source.isFile) compile(source, destination)
      else if (source.isDirectory) source.listFiles().foreach { f => if (f.isFile) compile(f, destination) }
      else print(source, "Illegal file", Console.RED)
    case _ => throw new IllegalArgumentException(s"Invalid argument number (${args.length}), but 2 expected")
  }

}