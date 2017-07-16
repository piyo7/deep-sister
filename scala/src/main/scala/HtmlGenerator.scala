import java.io.PrintWriter
import java.net.URLEncoder

import scala.collection.mutable
import scala.io.{Codec, Source}

object HtmlGenerator {
  def main(args: Array[String]) {
    val source = (for {fileName <- Seq(
      "tutorial.txt",
      "01_伊宮奉双譜.txt",
      "02_灼け焦がれた涙.txt",
      "03_魔法少女ふわふわの夏.txt",
      "04_流星シンドローム.txt",
      "05_DELETEME.txt",
      "06_蓬莱にごり酒.txt",
      "07_風里殺霊事件.txt",
      "credit.txt",
      "XX_「わたくしの一生を賭けて、あなたの幸せを計算してみせて？」.txt"
    )} yield Using(Source.fromResource(fileName)(Codec("UTF-8")))(_.mkString)).mkString

    val novel = Novel.parse("深層の令妹 ζ(*ﾟｗﾟ)ζ", source)

    for (chapter <- novel.chapters) {
      println(chapter.title)
      println(chapter.sections.flatMap(_.paragraphs).collect { case v: Voice => v.character }.distinct.mkString(", "))
      println()
    }

    val template = Using(Source.fromResource("template.html")(Codec("UTF-8")))(_.mkString)

    for ((chapter, nextChapter) <- novel.chapters.zipAll(novel.chapters.slice(1, 8).map(Some(_)), novel.chapters.last, None)) {
      val chat = (for ((section, i) <- chapter.sections.zipWithIndex) yield {
        (f"""<div class="section-index" id="${i + 1}%02d">""" +
          (if (i > 0) f"""<span>${if (section.title.take(2) == ": ") section.title.drop(2) else "§"}</span>""" else "") +
          """</div>""") +:
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
                ).flatten

                val lines = v.lines.init :+
                  (v.lines.last + (v.lines.last.lastOption match {
                    case Some(last) if (last.toString.getBytes.length > 1) && !periods.contains(last) => "。"
                    case _ => ""
                  }))

                for {
                  l <- if (v.character != 2) {
                    for {
                      line <- lines
                      l <- splitVoice(line)
                    } yield l
                  } else {
                    Seq((lines.init.map(line => line + (if (line.lastOption.contains('。')) "" else "　")) :+ lines.last).mkString)
                  }
                } yield {
                  f"""<p class="${classes.mkString(" ")}">${formatRuby(formatVoice(l))}</p>"""
                }

              case (d: Description, _) =>
                val classes = Seq("description") ++ (if (d.grayed) Seq("grayed") else Seq())
                Seq(f"""<p class="${classes.mkString(" ")}">${formatRuby(d.line.replace("<", "&lt").replace(">", "&gt"))}</p>""")

              case (Horizon, _) =>
                Seq("""<hr>""")

              case (Continued, _) =>
                Seq()

              case (Blank, Section.Kind.Voice) =>
                Seq("""<p class="voice-blank"><br></p>""")

              case (Blank, Section.Kind.Description) =>
                Seq("""<p class="description"><br></p>""")
            }
          }).flatten
      }).flatten

      val index = (if (chapter.sections.size > 1) {
        ("""<ul id="menu-index">""" +:
          (1 to chapter.sections.size).map(i => f"""<li><a class="hoverable" href="#$i%02d">$i</a></li>""").map(" " * 2 + _) :+
          """</ul>"""
        ).mkString("\n" + " " * 6)
      } else "")

      val lead = chapter.sections.flatMap(_.paragraphs).collect {
        case Voice(_, _, Voice.Kind.Direct, lines) => "「" + lines.mkString("") + "」"
        case Voice(_, _, Voice.Kind.Telephone, lines) => "『" + lines.mkString("") + "』"
      }.mkString.lines.mkString.
        replace("｜", "").
        replaceAll("《.*?》", "").
        replaceAll("<.*?>", "").
        take(199) + "…"

      val share = URLEncoder.encode(chapter.title + " - " + novel.title, "UTF-8").replace("+", "%20")

      val end = if (chapter.sections.last.paragraphs.last == Continued) {
          "（つづく）"
        } else {
          "（了）"
        }

      val next = nextChapter.map(c => f"""<a class="hoverable" href="${c.path}"><i class="fa fa-volume-control-phone"></i>『${c.title}』</a>""").
        getOrElse("""<a class="hoverable" href="./"><i class="fa fa-home"></i>『目次』</a>""")

      Using(new PrintWriter("../docs/" + chapter.path + ".html", "UTF-8")) {
        _.write(template.
          replace("__ROOT__", "https://piyo7.github.io/deep-sister").
          replace("__PATH__", chapter.path).
          replace("__NOVEL__", novel.title).
          replace("__CHAPTER__", chapter.title).
          replace("__MENU__", chapter.title).
          replace("__INDEX__", index).
          replace("__LEAD__", lead).
          replace("__CHAT__", chat.mkString("\n" + " " * 4)).
          replace("__SHARE__", share).
          replace("__END__", end).
          replace("__NEXT__", next))
      }
    }
  }

  private val periods = "。！？♪♡♥　"
  private val voiceWidth = 18

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

    for {
      ml1 <- recursiveSpan(markedLine)(_ == ('　', false)) if ml1.map(_._1) != Seq('　')
      mls = recursiveSpan(ml1)(pair => periods.contains(pair._1) && !pair._2)
      ml2 <- recursiveSplit(mls, voiceWidth * 2)(ml => ml.size <= 2 || ml.map(_._1).headOption.exists(periods.contains(_)))
    } yield ml2.map(_._1).mkString
  }

  def recursiveSpan[A](seq: Seq[A])(p: A => Boolean): Seq[Seq[A]] = {
    val (l, r) = seq.span(p)
    (if (l.isEmpty) Seq() else Seq(l)) ++ (if (r.isEmpty) Seq() else recursiveSpan(r)(!p(_)))
  }

  def recursiveSplit[A](seqs: Seq[Seq[A]], thresholdSize: Int)(extension: Seq[A] => Boolean): Seq[Seq[A]] = {
    seqs.map(_.size).scan(0)(_ + _).indexWhere(_ > thresholdSize) match {
      case i if i > 0 =>
        seqs.drop(i).indexWhere(!extension(_)) match {
          case j if j >= 0 =>
            val (l, r) = seqs.splitAt(i + j)
            Seq(l.flatten) ++ recursiveSplit(r, thresholdSize)(extension)
          case _ =>
            Seq(seqs.flatten)
        }
      case _ =>
        Seq(seqs.flatten)
    }
  }

  def formatVoice(line: String): String =
    if ((countNonRuby(line) <= voiceWidth + 1) && !line.init.exists(periods.contains(_)) && line.lastOption.contains('。')) {
      line.init
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
    map(c => if (c.toString.getBytes.length > 1) 1 else 0.5).
    sum.ceil.toInt
}
