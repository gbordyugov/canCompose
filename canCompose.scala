object CanCompose {
  class Trie(val terminal: Boolean, val children: Map[Char,Trie])

  object Trie {
    val empty = new Trie(true, Map())

    def apply(ss: List[String]): Trie =
      new Trie(true, ss.foldLeft(empty)(addString).children)

    def addString(t: Trie, s: String): Trie = {
      if (s.isEmpty)
        new Trie(true, t.children)
      else {
        val subt = t.children.getOrElse(s.head, {empty})
        val chds = t.children + (s.head -> addString(subt, s.tail))
        new Trie(false, chds)
      }
    }
  }

  def canCompose(s: String, dict: List[String]): Boolean = {
    val trie = Trie(dict)

    def go(s: String, t: Trie, stack: List[String]): Boolean = {
      if (s.isEmpty) true
      else if (!t.children.contains(s.head)) {
        if (stack.isEmpty) false
        else go(stack.head, trie, stack.tail)
      } else {
        val subt = t.children(s.head);
        if (subt.terminal) go(s.tail, subt, s.tail::stack)
        else               go(s.tail, subt,         stack)
      }
    }
    go(s, trie, List(s))
  }

  val test1 = canCompose("bla",         List("blu", "bli", "bla", " "))
  val test2 = canCompose("bla bli blu", List("blu", "bli", "bla", " "))
  val test3 = canCompose("quick brown fox",
    List("quick", "brown 1", "brow", "qui", "ck ", "n fox1", "n fox"))
}
