package lectures.collections

import scala.util.Random

/**
  * В этот задании Вам предстоит работать с очень неестабильным внешним сервисом.
  *
  * Для успешного завершения задания, вы должны реализовать метод buisnessLogic в объекте OptionVsNPE
  * Этот метод должен делать следующее
  * * * * * Получить и распечатать результат или, если была ошибка ResourceException,
  * распечатать "Try again with new resource" и повторить все заново
  * * * * * Получить ресурс через ResourceProducer
  * * * * * Если ресурс не получен, кидать ResourceException (throw new ResourceException)
  * * * * * Если ресурс удачно получен, на его основе получить Connection
  * * * * * Если соединение не получено, пробовать, пока соединение не будет получено
  * * * * * Вызвать у соединения метод result()
  * * * * * Если метод result возвращает Null, заменить его дефолтным сообщением из ОБъекта Connection
  *
  * Для успешного решения задания
  * * * * * Замените знаки вопроса реализацией методов
  * * * * * Нельзя менять, содержимое объектов ConnectionProducer, ResourceProducer
  * * * * * Не должно быть ни одного NPE
  * * * * * Можно менять входные и выходные параметры методов Connection
  *
  * трейт FailUtil - содержит методы для эмуляции спародических ошибок
  *
  *
  */
class ResourceException extends Exception("Ресурс не отвечает")

trait FailUtil {
  val failRate: Double

  def timeToFail = Math.random() > failRate
}

object ResourceProducer extends FailUtil {
  def produce = if (timeToFail) null else Resource(Random.alphanumeric.take(10).mkString)

  val failRate: Double = 0.3
}

object ConnectionProducer extends FailUtil {
  val failRate: Double = 0.5

  def produce(resource: Resource) = if (timeToFail) null else Connection(resource)

  def result(connection: Connection) = if (timeToFail) null else connection.resource.name
}

case class Connection(resource: Resource) {
  val defaultResult = "something went wrong!"

  //ConnectionProducer.result(this)
  def result(): String = ConnectionProducer.result(this)
}

case class Resource(name: String)

object OptionVsNPE extends App {

  def businessLogic: String = try {
    val resource = Option(ResourceProducer.produce)
    if (resource.isEmpty) throw new ResourceException
    val connection = getConnection(resource)

    val result: String = (Option(connection.result()) orElse Option(new Connection(resource.get).defaultResult)).get
    println(result)
    result
  } catch {
    case e: ResourceException => {
      println("Try again with new resource")
      businessLogic
    }
  }

  def getConnection(resource: Option[Resource]): Connection = {
    val connection = Option(ConnectionProducer.produce(resource.get))
    if (connection.isEmpty) getConnection(resource)
    else connection.get
  }

  businessLogic
}
