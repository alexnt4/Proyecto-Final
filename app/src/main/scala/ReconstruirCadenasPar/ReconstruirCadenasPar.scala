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

  def PRC_IngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    def generarSecuenciasRec(seq: Seq[Char], remaining: Int): LazyList[Seq[Char]] = {
      if (remaining == 0) LazyList(seq)
      else alfabeto.par.flatMap(char => generarSecuenciasRec(seq :+ char, remaining - 1)).to(LazyList)
    }

    val secuencias = generarSecuenciasRec(Seq.empty, n).par

    def buscarEnSecuencia(secuencia: Seq[Char]): Boolean = {
      o(secuencia)
    }

    if (n <= umbral) {
      ReconstruirCadenas.PRC_Ingenuo(n, o)
    } else {
      val resultados = secuencias.filter(buscarEnSecuencia).seq
      resultados.find(_.nonEmpty).getOrElse(Seq.empty[Char])
    }
  }

  

  
}
