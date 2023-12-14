/**
  * Proyecto final - Programación Funcional y concurrente
  * Autores: Alex García Castañeda - 2259517
 *           Sebastián Gómez Agudelo - 2259474
 *           Stiven Henao Aricapa - 2259603
  * Profesor: Carlos A Delgado
  */
package ReconstruirCadenas

import org.scalameter.{Warmer, withWarmer}
import common._
import Benchmark._
import Oraculo._

import scala.collection.parallel.CollectionConverters._
import scala.util.Random


object ReconstruirCadenas{

  def saludo() = "Proyecto Final 2023-II"

  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean


  
  def PRC_Ingenuo(n: Int, o: Oraculo): Seq[Char] = {
    def generarSecuencias(n: Int, s: Seq[Char]): LazyList[Seq[Char]] = {
     if (n == 0) LazyList(s)
      else alfabeto.to(LazyList).flatMap(char => generarSecuencias(n - 1, s :+ char))
    }

    val resultado = generarSecuencias(n, Seq.empty).find(o)
    resultado.getOrElse(Seq.empty)
  }






  // 3.3 implementando solución turbo
  
  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    def crearCadenaTurbo(tamano: Int, subActuales: Set[Seq[Char]]): Seq[Char] = {
      // crea las subcadenas con el doble de tamaño
      val nuevasSubcadenas = subActuales.flatMap(sub1 => subActuales.map(sub2 => sub1 ++ sub2))
      val subcadenasPrueba = nuevasSubcadenas.filter(o)

      // si encuentra una subcadena de tamaño n, la retorna, de lo contrario, sigue buscando
      subcadenasPrueba.find(_.length == n).getOrElse {
        // si no encuentra una subcadena de tamaño n, sigue buscando con el doble de tamaño
        if (tamano > n) Seq.empty[Char]
        else crearCadenaTurbo(tamano * 2, subcadenasPrueba)
      }
    }
    // hace las subcadenas con el alfabeto
    val subcadenasAlfabeto: Set[Seq[Char]] = alfabeto.map(Seq(_)).toSet
    crearCadenaTurbo(2, subcadenasAlfabeto)
  }

  


  def main(args: Array[String]): Unit = {
    println(saludo())
  }
 }
