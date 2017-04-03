package lectures.operators

/**
  * Проходит чемпионат по спортивному киданю костей)
  * Сражаются "Наши" и "Приезжие"
  *
  * Каждый член команды бросил кубик и должен сравнить свой результат с каждым результатом из команды соперника
  *
  * Итог сравнений должн быть записан в ассоциативный массив в таком виде
  * val results: Array[String] = (("Artem vs John" -> 3), ("Artem vs James" -> 5), ... )
  * При этом, числовове значение должно быть получено как разность между результатами первого и второго игроков
  *
  * Когда составлен массив results, надо подсчитать чья взяла.
  * Если результат встречи >0, то finalResult увеличивается на единицу
  * Если <0, уменьшается
  *
  * В итоге надо напечатать
  * "Наша взяла", если наших побед больше, т.е. finalResult > 0
  * "Продули", если победили приезжие
  * "Победила дружба" в случае ничьей
  *
  * Для решения задачи, раскомментируйте тело объекта Competition
  */

object Competition extends App {

  val locals = Map("Artem" -> 6, "Sergey" -> 5, "Anton" -> 2, "Vladimir" -> 2, "Alexander" -> 41)
  val foreigners = Map[String, Int]("John" -> 3, "James" -> 1, "Tom" -> 2, "Dick" -> 5, "Eric" -> 6)

    val results = for (l <- locals;
                       f <- foreigners) yield {
      val localName = l._1
      val foreignName = f._1
      val localValue = l._2
      val foreignValue = f._2
      (localName + " vs " + foreignName) -> (localValue - foreignValue)
    }

    var finalResult = 0
    for(r <- results) {
      if(r._2 > 0)finalResult = finalResult + 1
      else if (r._2 < 0) finalResult -= 1
    }
  if (finalResult == 0) print("Победила дружба")
  if (finalResult > 0) print("Наша взяла")
  if (finalResult < 0) print("Продули")
}
