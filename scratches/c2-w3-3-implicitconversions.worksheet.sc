/*
 *  JSON Example
 */

sealed trait Json
case class JNumber(value: BigDecimal) extends Json
case class JString(value: String) extends Json
case class JBoolean(value: Boolean) extends Json
case class JArray(elems: List[Json]) extends Json
case class JObject(fields: (String, Json)*) extends Json

// { "name": "Paul", "age": 42 }
JObject("name" -> JString("Paul"), "age" -> JNumber(42))

object Json {
  import scala.language.implicitConversions
  implicit def stringToJson(s: String): Json = JString(s)
  implicit def intToJson(n: Int): Json = JNumber(n)

  def obj(fields: (String, Json)*): Json = JObject(fields: _*)
}

("name" -> "Paul")
"age" -> 42
Json.obj("name" -> "Paul", "age" -> 42)

/*
 * Extension Methods
 */

import java.util.concurrent.TimeUnit

case class Duration(value: Int, unit: TimeUnit)
val delay = Duration(15, TimeUnit.SECONDS)

object Duration {

    object Syntax {
        import scala.language.implicitConversions
        implicit class HasSeconds(n: Int) {
            def seconds: Duration = Duration(n, TimeUnit.SECONDS)
        }
    }
}

import Duration.Syntax._
val delay2 = 15.seconds 



case class Rational(numerator: Int, denominator: Int)

object Rational {
  implicit def fromInt(n: Int) = Rational(n, 1)
}

val r: Rational = 42
println(r)