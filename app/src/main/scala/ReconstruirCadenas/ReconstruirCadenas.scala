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


// 3.2 Solución Mejorada
  /**
   * Función que reconstruye una cadena usando la propiedad de que s i s=s1++s2
   * entonces s1 y s2 tambien son subsecuencias de s.
   * @param n longitud de las secuencia que hay que reconstruir
   * @param o Oráculo utilizado para validar las subcadenas.
   * @return La cadena reconstruida que cumpla con la condición del oráculo.
   */
  def SolucionMejorada(n: Int, o: Oraculo): Seq[Char] = {
    //si n es menor que 1 retornará una seq vacía
    if (n < 1) {
      Seq.empty[Char] //Secuencia vacia
    } else { //En caso contrario se implementará
      def subcadenas(alfabeto: Seq[Char], longitud: Int): Seq[Seq[Char]] = {
        if (longitud == 0) //Si la longitud es 0 significa que la seq está sin elementos
          Seq(Seq.empty[Char])
        else {
          //Si no se buscará en las subcadenas anteriores
          val subcadenas_validas_anteriores = subcadenas(alfabeto, longitud - 1)
          //Se forman las combinaciones
          val lista_combinaciones = for (
            un_caracter <- alfabeto;
            una_subcadena <- subcadenas_validas_anteriores;
            cadenas_candidatas = un_caracter +: una_subcadena
            if o(cadenas_candidatas)
          ) yield cadenas_candidatas
          lista_combinaciones
        }
      }

      subcadenas(alfabeto, n).headOption.getOrElse(Seq.empty[Char])
      /*
      La función headOption devuelve el primer elemento de la secuencia
      como un Option, y getOrElse(Seq.empty[Char]) se asegura de que,
      si la secuencia está vacía, se devuelva una secuencia vacía por defecto.
      */
    }
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

  // 3.4 implementacion de la solucion turbo mejorada
  /**
   * Función que reconstruye una cadena usando Usa la propiedad de que s i s=s1++s2 entonces
   * s1 y s2 tambien son subsecuencias de s
   * @param n longitud de la secuencia que hay que reconstruir (n , potencia de 2)
   * @param o Oráculo utilizado para validar las subcadenas.
   * @return devuelve la secuencia reconstruida
   */
  def TurboMejorado(n: Int, PezOraculo: Oraculo): Seq[Char] = {
    //Evaluar si el tamaño es menor que 1 para retornar cadena vacia
    if (n < 1) {
      Seq.empty[Char]
    }
    else {
      // Funcion que verifica si una secuencia es subsecuencia de otra
      def verificarSecuencias(sec: Seq[Char], subsecuencias: Seq[Seq[Char]], l: Int): Boolean = {
        if (sec.length == l + 1) {
          true //Si es subsecuencia
        } else {
          val probar = sec.slice(1, 1 + l) //Para evaluar
          if (subsecuencias.contains(probar)) {
            verificarSecuencias(sec.drop(1), subsecuencias, l) //Se manda la recursion
          } else {
            false
          }
        }
      }

      //Funcion encargada de generar las cadenas validas anteriores para concatenarlas
      // y verificar si son subsecuencias de la cadena anterior y del oraculo
      def subcadenasTurbo(alfabeto: Seq[Char], longitud: Int): Seq[Seq[Char]] = {
        if (longitud == 1)
          for (un_caracter <- alfabeto; if (PezOraculo(Seq(un_caracter)))) yield Seq(un_caracter)
        else {
          val SubCadenasBuenasAnteriores = subcadenasTurbo(alfabeto, longitud / 2)
          val Combinaciones = for (
            sub_cadena_1 <- SubCadenasBuenasAnteriores;
            sub_cadena_2 <- SubCadenasBuenasAnteriores;
            valores = sub_cadena_1 ++ sub_cadena_2
            if (verificarSecuencias(valores, SubCadenasBuenasAnteriores, longitud / 2))
            if PezOraculo(valores)
          ) yield valores
          Combinaciones
        }
      }

      val cadenas = subcadenasTurbo(alfabeto, n)
      cadenas.headOption.getOrElse(Seq.empty[Char])
      /*
      Se usa el headOption para manejar el caso en que la
      secuencia es vacía y también se ha corregido la
      implementación de subcadenas en la función paralelizada
      para evitar posibles problemas de ordenamiento.
       */
    }
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

println("-----------------------------------------------------------------------------------------------\n\n")
    println("Comparacion de rendimiento entre SolucionMejoradaPar  y SolucionMejorada")
    val resultados2 = for {
      i <- 1 to 8 // Cambiando el rango para representar potencias de 2 de 2 a 16
      // Creación del oráculo y la cadena objetivo
      cadenaObjetivo = secAlAzar(i, Seq.empty)
      oraculo = Oraculo.crearOraculo(cadenaObjetivo)
      // Resultados de PRC_TurboAcelerada y PRC_TurboAceleradaPar
      resultado = SolucionMejorada(i, oraculo)
      resultadoPar = ReconstruirCadenasPar.SolucionMejoradaPar(4)(i, oraculo)
    } yield {
      println(s"Cadena objetivo para tamaño $i: $cadenaObjetivo")
      // Comparación con la cadena objetivo
      val coinciden = resultado == cadenaObjetivo && resultadoPar == cadenaObjetivo
      println(resultado, resultadoPar)
      println(s"Resultados coinciden para tamaño $i: $coinciden")
      (compararAlgoritmos(SolucionMejorada, ReconstruirCadenasPar.SolucionMejoradaPar(4))(i, oraculo), "Tamaño: " + i)
    }
    resultados2.foreach(println)

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

println("Comparacion de rendimiento entre Turbomejorada y TurbomejoradaPar")
    val resultados4 = for {
      i <- 1 to 8 // Cambiando el rango para representar potencias de 2 de 2 a 16
      size = math.pow(2, i).toInt // Calculando la potencia de 2
      // Creación del oráculo y la cadena objetivo
      cadenaObjetivo = secAlAzar(size, Seq.empty)
      oraculo = Oraculo.crearOraculo(cadenaObjetivo)
      // Resultados de PRC_TurboAcelerada y PRC_TurboAceleradaPar
      resultadoTurbo = TurboMejorado(size, oraculo)
      resultadoTurboPar = ReconstruirCadenasPar.TurboMejoradoPar(4)(size, oraculo)
    } yield {
      println(s"Cadena objetivo para tamaño $size: $cadenaObjetivo")
      // Comparación con la cadena objetivo
      val coinciden = resultadoTurbo == cadenaObjetivo && resultadoTurboPar == cadenaObjetivo
      println(resultadoTurbo, resultadoTurboPar)
      println(s"Resultados coinciden para tamaño $size: $coinciden")
      (compararAlgoritmos(TurboMejorado, ReconstruirCadenasPar.TurboMejoradoPar(4))(size, oraculo), "Tamaño: " + size)
    }
    resultados4.foreach(println)

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
