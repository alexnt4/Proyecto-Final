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


  // implementación de la solución turbo paralela

  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo) : Seq [Char]= {
    if (n <= umbral) {
      // Si el tamaño del conjunto de secuencias es menor o igual al umbral, ejecutar de forma secuencial
      ReconstruirCadenas.reconstruirCadenaTurbo(n, o)
    } else {
      def crearCadenaTurbo(tamano: Int, subActuales: ParSet[Seq[Char]]): Seq[Char] = {
        val nuevasSubcadenas = subActuales.flatMap(sub1 => subActuales.map(sub2 => sub1 ++ sub2)).par
        val subcadenasPrueba = nuevasSubcadenas.filter(o)

        // Obtener la primera subcadena válida de longitud N
        subcadenasPrueba.find(_.length == n).getOrElse {
          // Si el tamaño actual supera N, devolver una cadena vacía
          if (tamano > n) Seq.empty[Char]
          else crearCadenaTurbo(tamano * 2, subcadenasPrueba.par)
        }
      }
      // Inicializar el conjunto de subcadenas generadas de longitud 1 como ParSet
      val subcadenasAlfabeto: ParSet[Seq[Char]] = alfabeto.map(Seq(_)).toSet.par

      crearCadenaTurbo(2, subcadenasAlfabeto)
    }
  }

  

  
}
