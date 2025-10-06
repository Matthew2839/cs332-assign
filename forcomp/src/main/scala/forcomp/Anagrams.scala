package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *  
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *  
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurence list.
   *  
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {
    w.toLowerCase().filter(_.isLetter)
      .groupBy((element: Char) => element)
      .map{case(k, v) => (k, v.length)}
      .toList.sortBy(_._1)
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    wordOccurrences(s.mkString)
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *  
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    dictionary.groupBy((element: Word) => wordOccurrences(element))
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrences.getOrElse(wordOccurrences(word), Nil)
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   * 
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(Nil)
    case (ch, n) :: tail => {
      val tailComb = combinations(tail)
      val heads = (0 to n).toList.map(k => if (k == 0) Nil else List((ch, k)))
      for {
        h <- heads
        t <- tailComb
      } yield h ++ t
    }
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   * 
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val yMap = y.toMap.withDefaultValue(0)
    val ans = x.foldLeft(List.empty[(Char, Int)]) {
      case (acc, (k, v)) => (k, (v - yMap(k))) :: acc
    }
    ans.filter(_._2 != 0 ).sortBy(_._1)
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *  
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *  
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *  
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    val occ0 = sentenceOccurrences(sentence)
    def loop(occ: Occurrences): List[Sentence] =
      if (occ.isEmpty) List(Nil)
      else {
        for {
          comb   <- combinations(occ)
          if comb.nonEmpty
          words  <- dictionaryByOccurrences.get(comb).toList
          word   <- words
          rest   <- loop(subtract(occ, comb))
        } yield word :: rest
      }

    loop(occ0)
  }

}
// combinations를 만들 수 있는(사전에 있는) 단어조합으로만 구성되게 필터링한다.
// 첫 번째 경우를 고르고, 나머지들에 대해 재귀적인 연산을 한다.
// 재귀 과정에서 나머지를 필터링할 때,
// 1. 나머지가 없을 경우 - 완벽하게 정리됐으므로 마무리한다.
// 2. 나머지가 있는데 필터링해서 없어졌을 경우 - 아나그램 생성 불가로 해당 경로는 전부 제거한다.
// 3. 나머지가 있고 필터링해서 있을 경우 - 또 첫 번째 경우를 고르고, 1이나 2가 될 때까지 나머지들에 대해 재귀적인 연산을 한다.
// head는 0부터 끝까지 조합 중에 하나 골라서 for yield로 모든 단어 만들고, tail은 head 에 들어간거 subtract 해서 재귀연산
// sentenceOcc에서 Nil을 제외하고, 하나씩 subtract하면서 남은 거에서 또 0 to N으로 subtract하면서 모든 경우를 만든다.