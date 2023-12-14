package ReconstruirCadenasPar

import org.scalameter.{Warmer, withWarmer}
import common._
import Benchmark._
import Oraculo._
import ReconstruirCadenas._

import scala.collection.parallel.CollectionConverters._
import scala.util.Random

object ReconstruirCadenasPar {
  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean

  

  
}
