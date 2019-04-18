package homegrown

package object collections {
  type Stack[+Element] = List[Element]
  @inline val Stack: List.type = List
}
