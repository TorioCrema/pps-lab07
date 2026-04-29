package ex3

object Solitaire extends App:
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  type Mark = (Int, Int)
  type Solution = Iterable[Mark]
  type IterableFactory = Solution => Iterable[Solution]
  given IterableFactory = List(_)

  extension (m: Mark)
    def +(n: Mark): Mark = (m._1 + n._1, m._2 + n._2)

  private val validMoves: Seq[Mark] = (-3, 0) :: (-2, 2) :: (0, 3) :: (2, 2) :: (3, 0) :: (2, -2) :: (0, -3) :: (-2, -2) :: Nil

  def placeMarks(w: Int, h: Int): Iterable[Solution] = findSolutions(w * h)(w, h)

  def findSolutions(n: Int)(w: Int, h: Int)(using factory: IterableFactory): Iterable[Solution] = n match
    case 1 => factory(Seq((w / 2, h / 2)))
    case _ =>
      for
        marks <- findSolutions(n - 1)(w, h)
        (x, y) <- validMoves.map(marks.toSeq(marks.size - 1) + _)
        mark = (x, y)
        if isValid(mark, marks, w, h)
      yield marks.toSeq :+ mark


  private def isValid(mark: Mark, marks: Solution, w: Int, h: Int): Boolean =
    marks.forall(!mark.equals(_)) && mark._1 < w && mark._2 < h && mark._1 >= 0 && mark._2 >= 0

  for solution <- placeMarks(5, 5) do println(render(solution = solution.toSeq.reverse, width = 5, height = 5) + "\n")
