case class Novel(title: String, chapters: Seq[Chapter])

object Novel {
  def parse(title: String, source: String): Novel = {
    val chapters =
      for (chapter <- source.split("(?m)^# ") if chapter.trim.nonEmpty) yield {
        val titlePathSections = chapter.split("\n", 2)
        val titlePath = titlePathSections.head.split(" ", 2)
        Chapter.parse(titlePath.head, titlePath.last, titlePathSections.last)
      }
    Novel(title: String, chapters)
  }
}

case class Chapter(title: String, path: String, sections: Seq[Section])

object Chapter {
  def parse(title: String, path: String, source: String): Chapter = {
    val sections =
      for (section <- source.split("(?m)^## ").tail if section.trim.nonEmpty) yield {
        val titleLines = section.split("\n", 2)
        Section.parse(titleLines.head, titleLines.last)
      }
    Chapter(title, path, sections)
  }
}

case class Section(title: String, lines: Seq[Line]) {
  val kind: Section.Kind = title.headOption.map("「『".contains(_)) match {
    case Some(true) => Section.Kind.Voice
    case _ => Section.Kind.Description
  }
}

object Section {
  private val voiceR = """^([lr])(\d\d) ([「『])(.*)([」』])$""".r
  private val voiceStartR = """^([lr])(\d\d) ([「『])(.*)$""".r
  private val voiceEndR = """^(.*)([」』])$""".r
  private val commentR = """^::.*$""".r
  private val horizonR = """^---$""".r
  private val blankR = """^$""".r

  def parse(title: String, source: String): Section = {
    var parsingVoice: Option[Voice] = None

    val lines: Array[Line] =
      (for (line <- source.split("\n")) yield {
        (line, parsingVoice) match {
          case (voiceEndR(contentEnd, kindEnd), Some(voice@Voice(_, _, kindStart, _))) =>
            if (kindStart == Voice.Kind.parseEnd(kindEnd)) {
              parsingVoice = None
              Some(voice.copy(content = voice.content + contentEnd))
            } else {
              throw new IllegalArgumentException(contentEnd)
            }

          case (contentNext, Some(voice)) =>
            parsingVoice = Some(voice.copy(content = voice.content + contentNext + "\n"))
            None

          case (voiceR(position, character, kindStart, content, kindEnd), _) =>
            if (Voice.Kind.parseStart(kindStart) == Voice.Kind.parseEnd(kindEnd)) {
              Some(Voice(Voice.Position.parse(position), character.toInt, Voice.Kind.parseStart(kindStart), content))
            } else {
              throw new IllegalArgumentException(content)
            }

          case (voiceStartR(position, character, kindStart, content), _) =>
            parsingVoice = Some(Voice(Voice.Position.parse(position), character.toInt, Voice.Kind.parseStart(kindStart), content + "\n"))
            None

          case (commentR(), _) =>
            None

          case (horizonR(), _) =>
            Some(Horizon)

          case (blankR(), _) =>
            Some(Blank)

          case (content, _) =>
            Some(Description(content))
        }
      }).flatten

    Section(title, lines.dropWhile(_ == Blank).toSeq.reverse.dropWhile(_ == Blank).reverse)
  }

  sealed trait Kind

  object Kind {

    object Voice extends Kind

    object Description extends Kind

  }

}

sealed trait Line

case class Voice(position: Voice.Position, character: Int, kind: Voice.Kind, content: String) extends Line

object Voice {

  sealed trait Kind

  object Kind {
    def parseStart(source: String): Kind = source match {
      case "「" => Direct
      case "『" => Telephone
      case _ => throw new IllegalArgumentException
    }

    def parseEnd(source: String): Kind = source match {
      case "」" => Direct
      case "』" => Telephone
      case _ => throw new IllegalArgumentException
    }

    case object Direct extends Kind

    case object Telephone extends Kind

  }

  sealed trait Position

  object Position {
    def parse(source: String): Position = source match {
      case "l" => Left
      case "r" => Right
      case _ => throw new IllegalArgumentException
    }

    case object Left extends Position

    case object Right extends Position

  }

}

case class Description(content: String) extends Line

case object Horizon extends Line

case object Blank extends Line
