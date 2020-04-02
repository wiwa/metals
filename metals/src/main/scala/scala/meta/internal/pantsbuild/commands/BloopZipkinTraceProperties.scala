package scala.meta.internal.pantsbuild.commands

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.StandardOpenOption
import scala.meta.io.AbsolutePath

object BloopZipkinTraceProperties {

  private case class Property(metalsProperty: String) {

    val bloopProperty: String = metalsProperty.stripPrefix("metals.")

    def value: Option[String] = Option(System.getProperty(metalsProperty))
  }

  private val zipkinServerUrl = Property("metals.zipkin.server.url")

  private val localServiceName = Property("metals.bloop.trace.localServiceName")

  private val traceStartAnnotation = Property(
    "metals.bloop.trace.traceStartAnnotation"
  )

  private val traceEndAnnotation = Property(
    "metals.bloop.trace.traceEndAnnotation"
  )

  def updateBloopTraceProperties(): Boolean = {
    val properties = List(
      zipkinServerUrl,
      localServiceName,
      traceStartAnnotation,
      traceEndAnnotation
    )
    val oldOptions = readOptions(jvmopts)
    val newOptions = properties.foldLeft(oldOptions) { (options, prop) =>
      prop.value match {
        case Some(newValue) =>
          val oldValue = optionValue(options, prop.bloopProperty)
          if (!oldValue.contains(newValue)) {
            updateOption(options, prop.bloopProperty, newValue)
          } else {
            options
          }
        case None =>
          options
      }
    }
    if (newOptions != oldOptions) {
      Files.write(
        jvmopts.toNIO,
        newOptions.mkString("\n").getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.TRUNCATE_EXISTING,
        StandardOpenOption.CREATE
      )
      true
    } else {
      false
    }
  }

  private def jvmopts: AbsolutePath = {
    val homedir = AbsolutePath(System.getProperty("user.home"))
    homedir.resolve(".bloop").resolve(".jvmopts")
  }

  private def readOptions(jvmopts: AbsolutePath): List[String] = {
    import scala.meta.internal.metals.MetalsEnrichments._

    if (jvmopts.isFile) jvmopts.readText.linesIterator.toList
    else Nil
  }

  private def optionValue(
      options: List[String],
      key: String
  ): Option[String] = {
    val regex = s"-D$key=(.*)".r
    options.collectFirst { case regex(value) => value.stripPrefix(s"-D$key=") }
  }

  private def updateOption(
      options: List[String],
      key: String,
      newValue: String
  ): List[String] = {
    val otherOptions = options.filterNot(_.startsWith(s"-D$key="))
    val newOption = s"-D$key=$newValue"
    scribe.info(s"zipkin: setting $key=$newValue")
    newOption :: otherOptions
  }
}
