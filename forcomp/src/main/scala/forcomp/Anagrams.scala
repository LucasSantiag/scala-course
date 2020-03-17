package forcomp

object Anagrams extends AnagramsInterface {

  type Word = String

  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = Dictionary.loadDictionary

  def wordOccurrences(w: Word): Occurrences = (for (
    (c, occs) <- w.toLowerCase groupBy (c => c)
  ) yield (c, occs.length)).toList.sorted

  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString)

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary.groupBy(word => wordOccurrences(word)).withDefaultValue(List())

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences(wordOccurrences(word))

  def combinations(occurrences: Occurrences): List[Occurrences] =
    (occurrences foldRight List[Occurrences](Nil)) {
      case ((ch, occs), accCombs) =>
        (for {
          comb <- accCombs
          count <- 1 to occs
        } yield (ch, count) :: comb) ++ accCombs
    }

  def subtract(x: Occurrences, y: Occurrences): Occurrences = y.toMap.foldLeft(x.toMap)(
    (acc, pair) => acc.updated(pair._1, acc(pair._1) - pair._2)
  ).toList.filter(_._2 > 0).sorted


  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

    def sentenceOccurrAnagrams(occurrences: Occurrences): List[Sentence] = occurrences match {
      case Nil => List(Nil)
      case _ => for {
        comb <- combinations(occurrences)
        word <- dictionaryByOccurrences(comb)
        restSentence <- sentenceOccurrAnagrams(subtract(occurrences, comb))
      } yield word :: restSentence
    }

    sentenceOccurrAnagrams(sentenceOccurrences(sentence))
  }


}

object Dictionary {
  def loadDictionary: List[String] = {
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = scala.io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }
}
