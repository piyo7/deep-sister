import java.io.PrintWriter
import java.net.URLEncoder

import scala.io.Source

object HtmlGenerator {
  def main(args: Array[String]) {
    val source = (for {fileName <- Seq(
      "tutorial.md",
      "01_伊宮奉双譜.md",
      "02_灼け焦がれた涙.md",
      "03_魔法少女ふわふわの夏.md",
      "04_流星シンドローム.md",
      "05_DELETEME.md",
      "credit.md"
    )} yield Using(Source.fromResource(fileName))(_.mkString)).mkString

    val novel = Novel.parse("深層の令妹 ζ(*ﾟｗﾟ)ζ", source)

    for (chapter <- novel.chapters) {
      println(chapter.title)
      println(chapter.sections.flatMap(_.lines).collect { case v: Voice => v.character }.distinct.sorted.mkString(", "))
      println()
    }

    val template = Using(Source.fromResource("template.html"))(_.mkString)

    for ((chapter, nextChapter) <- novel.chapters.zipAll(novel.chapters.slice(1, 6).map(Some(_)), novel.chapters.last, None)
    ) {
      val chat = (for ((section, i) <- chapter.sections.zipWithIndex) yield {
        f"""<div class="sectionIndex" id="section-${i + 1}%02d"><a href="#section-${i + 1}%02d">§</a></div>""" +:
          (for (line <- section.lines) yield {
            (line, section.kind) match {
              case (v: Voice, _) =>
                for (l <- v.content.lines) yield {
                  val class1 = "voice-" + (v.position match {
                    case Voice.Position.Right => "right"
                    case Voice.Position.Left => "left"
                  })
                  val class2 = v.kind match {
                    case Voice.Kind.Telephone => "telephone"
                    case Voice.Kind.Direct => ""
                  }
                  val class3 = f"char${v.character}%02d"

                  f"""<p class="$class1 $class2 $class3">${formatRuby(formatVoice(l))}</p>"""
                }

              case (d: Description, _) =>
                Seq(f"""<p class="description">${formatRuby(d.content)}</p>""")

              case (Horizon, _) =>
                Seq("""<hr>""")

              case (Blank, Section.Kind.Voice) =>
                Seq("""<p class="voice-blank"><br></p>""")

              case (Blank, Section.Kind.Description) =>
                Seq("""<p class="description"><br></p>""")
            }
          }).flatten
      }).flatten

      val lead = chapter.sections.flatMap(_.lines).collect {
        case Voice(_, _, Voice.Kind.Direct, content) => "「" + content + "」"
        case Voice(_, _, Voice.Kind.Telephone, content) => "『" + content + "』"
      }.mkString.lines.mkString.
        replace("｜", "").
        replaceAll("《.*?》", "").
        replaceAll("<.*?>", "").
        take(199) + "…"

      val share = URLEncoder.encode(chapter.title + " - " + novel.title, "UTF-8")

      val next = nextChapter.map(c => f"""<a href="${c.path}">次へ</a>""").getOrElse("")

      Using(new PrintWriter("../docs/" + chapter.path + ".html", "UTF-8")) {
        _.write(template.
          replace("__ROOT__", "https://piyo7.github.io/deep-sister").
          replace("__PATH__", chapter.path).
          replace("__NOVEL__", novel.title).
          replace("__CHAPTER__", chapter.title).
          replace("__LEAD__", lead).
          replace("__CHAT__", chat.mkString("\n    ")).
          replace("__SHARE__", share).
          replace("__NEXT__", next))
      }
    }
  }

  private val periods = "。！？♪♡♥"

  def formatVoice(content: String): String = {
    content.dropWhile(_ == '　') +
      (if (!periods.contains(content.last) && periods.exists(content.contains(_))) "。" else "")
  }

  def formatRuby(content: String): String = content.
    replace("｜", "<ruby>").
    replace("《", "<rt>").
    replace("》", "</rt></ruby>")
}
