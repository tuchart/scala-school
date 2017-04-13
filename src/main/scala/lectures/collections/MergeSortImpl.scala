package lectures.collections

/**
  * Постарайтесь не использовать мутабильные коллекции и var
  * Подробнее о сортировке можно подсмотреть здесь - https://en.wikipedia.org/wiki/Merge_sort
  *
  *
  */
object MergeSortImpl extends App {

  def mergeSort(data: Seq[Int]): Seq[Int] = {
    val middle = data.length / 2
    if (middle == 0) data
    else {
      val (left, right) = data.splitAt(middle)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def merge(left: Seq[Int], right: Seq[Int]): Seq[Int] = (left, right) match {
    case (left, Nil) => left
    case (Nil, right) => right
    case (leftHead :: leftTail, rightHead :: rightTail) => {
      if (leftHead < rightHead) List(leftHead) ++ merge(leftTail, right)
      else List(rightHead) ++ merge(left, rightTail)
    }
  }


  val sorted = mergeSort(List(5, 7, 3, -6, 10, 1))
  println(sorted)

}
