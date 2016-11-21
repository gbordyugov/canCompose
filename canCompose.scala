object CanCompose {
  class Trie(val terminal: Boolean, val children: Map[Char,Trie])

  object Trie {
    val empty = new Trie(true, Map())

    def apply(ss: List[String]): Trie =
      new Trie(true, ss.foldLeft(empty)(addString).children)

    def addString(t: Trie, s: String): Trie = {
      if (s.isEmpty) new Trie(true, t.children)
      else {
        val subt = t.children.getOrElse(s.head, {empty})
        val chds = t.children + (s.head -> addString(subt, s.tail))
        new Trie(false, chds)
      }
    }
  }

  def canCompose(s: String, dict: List[String]): Boolean = {
    val trie = Trie(dict)

    /*
     * as if strings were lists with fast head and tail methods
     */
    def go(s: String, t: Trie, stack: List[String]): Boolean = {
      if (s.isEmpty)
        if (t.terminal)
          true
        else
          if (stack.isEmpty)
            false
          else
            go(stack.head, trie, stack.tail)
      else
        if (!t.children.contains(s.head))
          if (stack.isEmpty)
            false
          else
            go(stack.head, trie, stack.tail)
      else {
        val subt = t.children(s.head)
        if (subt.terminal)
          go(s.tail, subt, s.tail::stack)
        else
          go(s.tail, subt,         stack)
      }
    }

    /*
     * int index-based solution
     */
    def goInt(i: Int, t: Trie, stack: List[Int]): Boolean = {
      if (i == s.length)
        if (t.terminal)
          true
        else
          if (stack.isEmpty)
            false
          else
            goInt(stack.head, trie, stack.tail)
      else
        if (!t.children.contains(s(i)))
          if (stack.isEmpty)
            false
          else
            goInt(stack.head, trie, stack.tail)
      else {
        val subt = t.children(s(i))
        if (subt.terminal)
          goInt(i+1, subt, i+1::stack)
        else
          goInt(i+1, subt,      stack)
      }
    }
    go(s, trie, List(s))
    // goInt(0, trie, List(0))
  }


  def tests(): Unit = {
    assert(canCompose("bla",         List("blu", "bli", "bla", " ")))
    assert(canCompose("bla bli blu", List("blu", "bli", "bla", " ")))
    assert(canCompose("quick brown fox", List("quick", "brown 1", "brow",
                                              "qui", "ck ", "n fox1",
                                              "n fox")))
    assert(!canCompose("bla", List("bla1")))

    assert(canCompose("abc", List("a", "b", "c")))
    assert(canCompose("aaa", List("a", "b")))
    assert(canCompose("foo", List("foo")))
    assert(canCompose("quick brown fox",
                       List("quick ", "own", "br", " fo", "ox", " f")))
    assert(canCompose("", List("")))
    assert(canCompose("", List(" ")))

    assert(!canCompose("cat loves read", List("dog", "loves", " ", "read")))
    assert(!canCompose("longread", List("long", "gread")))
    assert(!canCompose("longread", List("longread1")))

    assert(canCompose("longread",
                      List("long", "gread", "lo", " ng", "re", "ad")))
    assert(canCompose("abc", List("abcd", "ab", "c")))
    println("All tests passed")
  }
}
