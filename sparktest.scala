import scala.reflect.runtime.universe._
import scala.util.Random
import scala.math._

object Main extends App {

  def percentile[T](x: List[T], p: Double)(implicit tag: TypeTag[T]): Option[T] = {

    require(p < 1 && p > 0)
    require(x.length > 0)

    x match {

      case value: List[Double] if typeOf[T] =:= typeOf[Double] => {

        val y = value.sorted
        Some(y(ceil(value.length*p)-1 toInt))
      }

      case value: List[Int] if typeOf[T] =:= typeOf[Int] => {

        val y = value.sorted
        Some(y(ceil(value.length*p)-1 toInt))
      }

      case _ => None
    }
  }

  val nums = 1000
  val input = Seq.fill(nums)(Random.nextGaussian).toList
  //val input = Seq.fill(nums)(Random.nextInt).toList
  //val input = List("abc")
  val res = percentile(input, .75)
  if (res == None) println("Error")
  else println("%.3f".format(percentile(input, .75).get))
}