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

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.util.Random
import ReconstruirCadenasPar._


object ReconstruirCadenas{

  def saludo() = "Proyecto Final 2023-II"

  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean

  /**
   * Crea una secuencia aleatoria de caracteres del alfabeto con una longitud determinada,
   * finalizando en una secuencia dada.
   *
   * @param long Longitud deseada de la secuencia aleatoria.
   * @param s    Secuencia con la que finalizará la secuencia aleatoria.
   * @return Una secuencia de caracteres aleatoria con la longitud especificada.
   */
  def secAlAzar(long: Int, s: Seq[Char]): Seq[Char] = {
    // Crea recursivamente una secuencia aleatoria hasta alcanzar la longitud deseada.
    val random = new Random()
    if (s.length == long) s
    else {
      val indiceAzar = random.nextInt(4)
      secAlAzar(long, alfabeto(indiceAzar) +: s)
    }
  }

  // 3.1 implementando solución ingenua
  /**
   * Método encargado de reconstruir una cadena de manera ingenua de forma secuencial.
   *
   * @param n Longitud de las subcadenas a generar.
   * @param o Oráculo utilizado para validar las subcadenas.
   * @return La subcadena que cumpla con la condición del oráculo.
   */
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    // Función interna que genera todas las secuencias posibles con longitud 'n'.
    def generarSecuencias(n: Int, s: Seq[Char]): LazyList[Seq[Char]] = {
      if (n == 0) LazyList(s) // Caso base: retorna la secuencia generada cuando alcanza la longitud deseada.
      else alfabeto.to(LazyList).flatMap(char => generarSecuencias(n - 1, s :+ char)) // Genera recursivamente las secuencias.
    }

    // Obtiene las secuencias generadas y encuentra la primera que cumple con el oráculo 'o'.
    val resultado = generarSecuencias(n, Seq.empty).find(o)
    resultado.getOrElse(Seq.empty) // Retorna la secuencia encontrada o una secuencia vacía si no hay coincidencias.
  }






  // 3.3 implementando solución turbo

  /**
   * Función que reconstruye una cadena usando un enfoque turbo de manera secuencial.
   *
   * @param n Longitud deseada de la cadena a reconstruir.
   * @param o Oráculo utilizado para validar las subcadenas.
   * @return La cadena reconstruida que cumpla con la condición del oráculo.
   */
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

  // 3.5 implementando solución turbo acelerada
  /**
   * Función que reconstruye una cadena usando un enfoque turbo acelerado de manera secuencial.
   *
   * @param n Longitud deseada de la cadena a reconstruir.
   * @param o Oráculo utilizado para validar las subcadenas.
   * @return La cadena reconstruida que cumpla con la condición del oráculo.
   */
  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    @tailrec
    def generarSecuenciasValidas(secuencias: Seq[Seq[Char]], arbol: ArbolSufijos.Trie, aumentador: Int): Seq[Char] = {
      // Verifica si alguna de las secuencias alcanza la longitud deseada 'n'.
      if (secuencias.exists(_.length == n)) {
        secuencias.find(_.length == n).getOrElse(Seq.empty[Char]) // Retorna la secuencia de longitud 'n' si se encuentra.
      } else {
        // Crea un nuevo árbol con las secuencias actuales y verifica combinaciones para generar nuevas secuencias válidas.
        val nuevoArbol = secuencias.foldLeft(arbol) { (arbolActual, secuencia) =>
          ArbolSufijos.adicionar(secuencia, arbolActual)
        }
        // Genera nuevas secuencias que cumplen con las restricciones del oráculo 'o'.
        val nuevasSecuencias = for {
          subSeq1 <- secuencias
          subSeq2 <- secuencias if subSeq1.length + subSeq2.length - 1 <= n
          valor = subSeq1 ++ subSeq2
          if (o(valor))
        } yield valor
        // Llama recursivamente para generar más secuencias válidas.
        generarSecuenciasValidas(nuevasSecuencias, nuevoArbol, aumentador * 2)
      }
    }

    // Genera una secuencia inicial filtrando el alfabeto con el oráculo.
    val secuenciaInicial = alfabeto.filter(c => o(Seq(c))).map(Seq(_))
    // Crea el árbol de sufijos basado en la secuencia inicial.
    val arbolInicial = ArbolSufijos.arbolDeSufijos(secuenciaInicial)
    // Inicia la generación de secuencias válidas.
    generarSecuenciasValidas(secuenciaInicial, arbolInicial, 1)
  }
  


  def main(args: Array[String]): Unit = {
    println(saludo())
    /*
    println("Comparación de rendimiento entre reconstruirCadenasIngenuo y reconstruirCadenasIngenuoPar")
    val resultados1 = for {
      i <- 1 to 8
      // Creación del oráculo y la cadena objetivo
      cadenaObjetivo = secAlAzar(i, Seq.empty)
      oraculo = Oraculo.crearOraculo(cadenaObjetivo)
      // Resultados de PRC_Ingenuo y PRC_IngenuoPar
      resultadoIngenuo = reconstruirCadenaIngenuo(i, oraculo)
      resultadoIngenuoPar = ReconstruirCadenasPar.reconstruirCadenaIngenuoPar(6)(i, oraculo)
    } yield {
      println(s"Cadena objetivo para tamano $i: $cadenaObjetivo")
      // Comparación con la cadena objetivo
      val coinciden = resultadoIngenuo == cadenaObjetivo && resultadoIngenuoPar == cadenaObjetivo
      println(s"Resultados coinciden para tamano $i: $coinciden")
      (compararAlgoritmos(reconstruirCadenaIngenuo, ReconstruirCadenasPar.reconstruirCadenaIngenuoPar(6))(i, oraculo), "Tamano: " + i)
    }
    resultados1.foreach(println)

    println("Comparación de rendimiento entre cadena turbo y  cadena turbo paralela")
    val resultados3 = for {
      i <- 1 to 4
      size = math.pow(2, i).toInt
      // Creación del oráculo y la cadena objetivo
      cadenaObjetivo = secAlAzar(size, Seq.empty)
      oraculo = Oraculo.crearOraculo(cadenaObjetivo)
      // Resultados de reconstruirCadenaTurbo y reconstruirCadenaTurboPar
      resultadoTurbo = reconstruirCadenaTurbo(size, oraculo)
      resultadoTurboPar = ReconstruirCadenasPar.reconstruirCadenaTurboPar(4)(size, oraculo)
    } yield {
      println(s"Cadena objetivo para tamano $size: $cadenaObjetivo")
      // Comparación con la cadena objetivo
      val coinciden = resultadoTurbo == cadenaObjetivo && resultadoTurboPar == cadenaObjetivo
      println(s"Resultados coinciden para tamano $size: $coinciden")
      (compararAlgoritmos(reconstruirCadenaTurbo, ReconstruirCadenasPar.reconstruirCadenaTurboPar(4))(size, oraculo), "Tamano: " + size)
    }
    resultados3.foreach(println)

    println("Comparación de rendimiento entre reconstruirCadenaTurboAcelerada y reconstruirCadenaTurboAceleradaPar")
    val resultados5 = for {
      i <- 1 to 8 // Cambiando el rango para representar potencias de 2 de 2 a 16
      size = math.pow(2, i).toInt // Calculando la potencia de 2
      // Creación del oráculo y la cadena objetivo
      cadenaObjetivo = secAlAzar(size, Seq.empty)
      oraculo = Oraculo.crearOraculo(cadenaObjetivo)
      // Resultados de PRC_TurboAcelerada y PRC_TurboAceleradaPar
      resultadoTurboAcelerada = reconstruirCadenaTurboAcelerada(size, oraculo)
      resultadoTurboAceleradaPar = ReconstruirCadenasPar.reconstruirCadenaTurboAceleradaPar(6)(size, oraculo)
    } yield {
      println(s"Cadena objetivo para tamano $size: $cadenaObjetivo")
      // Comparación con la cadena objetivo
      val coinciden = resultadoTurboAcelerada == cadenaObjetivo && resultadoTurboAceleradaPar == cadenaObjetivo
      println(resultadoTurboAcelerada, resultadoTurboAceleradaPar)
      println(s"Resultados coinciden para tamano $size: $coinciden")
      (compararAlgoritmos(reconstruirCadenaTurboAcelerada, ReconstruirCadenasPar.reconstruirCadenaTurboAceleradaPar(6))(size, oraculo), "Tamano: " + size)
    }
    resultados5.foreach(println)

     */

  }
 }
