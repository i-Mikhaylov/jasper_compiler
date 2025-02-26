import net.sf.jasperreports.engine.JasperCompileManager
import net.sf.jasperreports.engine.util.JRSaver
import net.sf.jasperreports.engine.xml.JRXmlLoader

import java.io.{ByteArrayInputStream, File}
import java.nio.file.{FileSystemException, Files}
import scala.util.{Failure, Success, Try}


object Main {

  implicit class PrintError[T](result: Try[T]) {
    def orPrint(message: Throwable => String): Option[T] = result match {
      case Success(value) => Some(value)
      case Failure(error) => println(message(error)); None
    }
  }

  def readConvertJrxml(source: File): Array[Byte] = {
    val origXml = Files.readAllBytes(source.toPath)
    val str = new String(origXml)

    val jsonLocal = "net.sf.jasperreports.json.data.JsonDataSource"
    val jsonServer = "net.sf.jasperreports.engine.data.JsonDataSource"
    val jsonReplaced = str.replace(jsonLocal, jsonServer)
    if (jsonReplaced != str) println(s"${source.getAbsolutePath}: Replaced \"$jsonLocal\" -> \"$jsonServer\"")

    jsonReplaced.getBytes
  }

  def compile(source: File, destination: File): Unit = {
    val convertedXml = readConvertJrxml(source)
    Files.write(new File(destination, source.getName).toPath, convertedXml)
    val input = new ByteArrayInputStream(convertedXml)
    val sourcePath = source.getAbsolutePath

    for {
      outputName <- Some(source.getName).collect { case s"$prefix.jrxml" => s"$prefix.jasper" }
      design <- Try { JRXmlLoader.load(input) }
        .orPrint(e => s"$sourcePath: Error loading report - ${e.getMessage}")
      report <- Try { JasperCompileManager.compileReport(design) }
        .orPrint(e => s"$sourcePath: Error compiling report - ${e.getMessage}")
    } yield Try { JRSaver.saveObject(report, new File(destination, outputName)) }
      .orPrint(e => s"$sourcePath: Error saving compiled report - ${e.getMessage}")
      .map { _ => println(s"$sourcePath: Compiled successfully") }
  }

  def main(args: Array[String]): Unit = args.map(new File(_)) match {
    case Array(source, destination) =>
      if (source.isFile) compile(source, destination)
      else if (source.isDirectory) source.listFiles().foreach { f => if (f.isFile) compile(f, destination) }
      else throw new FileSystemException(s"${source.getAbsolutePath}: Illegal file ")
    case _ => throw new IllegalArgumentException(s"Invalid argument number (${args.length}), but 2 expected")
  }

}