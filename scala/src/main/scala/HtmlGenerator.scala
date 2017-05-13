import java.io.PrintWriter
import java.net.URLEncoder

import scala.collection.mutable
import scala.io.Source

object HtmlGenerator {
  def main(args: Array[String]) {
    val source = (for {fileName <- Seq(
      "tutorial.txt",
      "01_伊宮奉双譜.txt",
      "02_灼け焦がれた涙.txt",
      "03_魔法少女ふわふわの夏.txt",
      "04_流星シンドローム.txt",
      "05_DELETEME.txt",
      "credit.txt"
    )} yield Using(Source.fromResource(fileName))(_.mkString)).mkString

    val novel = Novel.parse("深層の令妹 ζ(*ﾟｗﾟ)ζ", source)

    for (chapter <- novel.chapters) {
      println(chapter.title)
      println(chapter.sections.flatMap(_.paragraphs).collect { case v: Voice => v.character }.distinct.sorted.mkString(", "))
      println()
    }

    val template = Using(Source.fromResource("template.html"))(_.mkString)

    for ((chapter, nextChapter) <- novel.chapters.zipAll(novel.chapters.slice(1, 6).map(Some(_)), novel.chapters.last, None)) {
      val chat = (for ((section, i) <- chapter.sections.zipWithIndex) yield {
        (if (i > 0) f"""<div class="sectionIndex"><span>§</span></div>""" else "") +:
          (for (paragraph <- section.paragraphs) yield {
            (paragraph, section.kind) match {
              case (v: Voice, _) =>
                val classes = Seq(
                  v.position match {
                    case Voice.Position.Right => Some("voice-right")
                    case Voice.Position.Left => Some("voice-left")
                  },
                  v.kind match {
                    case Voice.Kind.Telephone => Some("telephone")
                    case Voice.Kind.Direct => None
                  },
                  Some(f"char${v.character}%02d")
                  , Some("voice")
                ).flatten

                for {
                  line <- v.lines
                  l <- splitVoice(line + (line.lastOption match {
                    case Some(last) if (last.toString.getBytes.length > 1) && !periods.contains(last) => "。"
                    case _ => ""
                  }))
                } yield {
                  f"""<p class="${classes.mkString(" ")}">${formatRuby(formatVoice(l))}</p>"""
                }

              case (d: Description, _) =>
                Seq(f"""<p class="description">${formatRuby(d.line)}</p>""")

              case (Horizon, _) =>
                Seq("""<hr>""")

              case (Blank, Section.Kind.Voice) =>
                Seq("""<p class="voice-blank"><br></p>""")

              case (Blank, Section.Kind.Description) =>
                Seq("""<p class="description"><br></p>""")
            }
          }).flatten
      }).flatten

      val lead = chapter.sections.flatMap(_.paragraphs).collect {
        case Voice(_, _, Voice.Kind.Direct, lines) => "「" + lines.mkString("") + "」"
        case Voice(_, _, Voice.Kind.Telephone, lines) => "『" + lines.mkString("") + "』"
      }.mkString.lines.mkString.
        replace("｜", "").
        replaceAll("《.*?》", "").
        replaceAll("<.*?>", "").
        take(199) + "…"

      val share = URLEncoder.encode(chapter.title + " - " + novel.title, "UTF-8").replace("+", "%20")

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

  private val periods = "。！？♪♡♥　"
  private val voiceWidth = 20

  def splitVoice(line: String): Seq[String] = {
    val bracket = mutable.Seq.fill(3)(false)
    val markedLine = for (char <- line) yield {
      char match {
        case '｜' => bracket(0) = true
        case '》' => bracket(0) = false
        case '「' => bracket(1) = true
        case '」' => bracket(1) = false
        case '『' => bracket(2) = true
        case '』' => bracket(2) = false
        case _ =>
      }
      (char, bracket.reduce(_ || _))
    }
    spanSeq(markedLine)(_ == ('　', false)).map(_.map(_._1).mkString).filterNot(_ == "　")
  }

  def spanSeq[A](seq: Seq[A])(p: A => Boolean): Seq[Seq[A]] = {
    val (l, r) = seq.span(p)
    (if (l.isEmpty) Seq() else Seq(l)) ++ (if (r.isEmpty) Seq() else spanSeq(r)(!p(_)))
  }

  def formatVoice(line: String): String =
    if ((countNonRuby(line) <= voiceWidth + 1) && !line.dropRight(1).exists(periods.contains(_)) && line.lastOption.contains('。')) {
      line.dropRight(1)
    } else {
      line
    }

  def formatRuby(line: String): String = line.
    replace("｜", "<ruby>").
    replace("《", "<rt>").
    replace("》", "</rt></ruby>")

  def countNonRuby(line: String): Int = line.
    replace("｜", "").
    replaceAll("《.*?》", "").
    length
}
