package scalaz
package xml
package pp

import QName._

sealed trait Config {
  val short_empty_tag: QName => Boolean
  val prettify: Option[Str]

  import Config._

  /// useShortEmptyTags
  def setShortEmptyTag(p: QName => Boolean): Config =
    config(p, prettify)

  /// useExtraWhiteSpace
  def setPrettify(p: Option[Str]): Config =
    config(short_empty_tag, p)

  def notShortEmptyTag: QName => Boolean =
    n => !short_empty_tag(n)

  def prettifyOn(s: Str): Config =
    config(short_empty_tag, Some(s))

  def prettifyOff: Config =
    config(short_empty_tag, None)

  def isPrettify: Boolean =
    prettify.isDefined

  def isNotPrettify: Boolean =
    prettify.isEmpty

  def shortEmptyTagOn: Config =
    config(_ => true, prettify)

  def shortEmptyTagOff: Config =
    config(_ => false, prettify)

  def toggleShortEmptyTag: Config =
    config(n => !short_empty_tag(n), prettify)

}

trait Configs {
  /// defaultConfigPP
  def config(short_empty_tag: QName => Boolean = _ => true, prettify: Option[Str] = None): Config = {
    val s = short_empty_tag
    val p = prettify
    new Config {
      val short_empty_tag = s
      val prettify = p
    }
  }

  /// prettyConfigPP
  val pretty: Config =
    config() prettifyOn (List.fill(2)(' '))

}

object Config extends Configs
