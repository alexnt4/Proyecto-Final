package ReconstruirCadenasPar

import org.scalameter.{Warmer, withWarmer}
import common._
import Benchmark._
import Oraculo._
import ReconstruirCadenas._
import scala.annotation.tailrec
import scala.collection.parallel.immutable.{ParSet, ParSeq}
import scala.collection.parallel.CollectionConverters._
import scala.util.Random

object ReconstruirCadenasPar {
  val alfabeto = Seq('a', 'c', 'g', 't')
  type Oraculo = Seq[Char] => Boolean

  // implementación de la solución ingenua paralela
  /**
   * Método encargado de reconstruir una cadena de manera ingenua de forma paralela si supera un umbral.
   *
   * @param umbral Valor límite para decidir si se aplica la reconstrucción de manera paralela.
   * @param n      Longitud de las subcadenas a generar.
   * @param o      Oráculo utilizado para validar las subcadenas.
   * @return La subcadena que cumpla con la condición del oráculo.
   */
  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // Función que genera recursivamente las secuencias con longitud 'n'.
    def generarSecuenciasRec(seq: Seq[Char], remaining: Int): LazyList[Seq[Char]] = {
      if (remaining == 0) LazyList(seq) // Caso base: retorna la secuencia generada cuando alcanza la longitud deseada.
      else alfabeto.par.flatMap(char => generarSecuenciasRec(seq :+ char, remaining - 1)).to(LazyList) // Genera recursivamente las secuencias de forma paralela.
    }

    // Genera las secuencias de manera paralela.
    val secuencias = generarSecuenciasRec(Seq.empty, n).par

    // Función que verifica si una secuencia cumple con el oráculo 'o'.
    def buscarEnSecuencia(secuencia: Seq[Char]): Boolean = {
      o(secuencia)
    }

    // Si n es menor o igual al umbral, llama a la función secuencial.
    if (n <= umbral) {
      ReconstruirCadenas.reconstruirCadenaIngenuo(n, o)
    } else {
      // Filtra las secuencias para encontrar la primera que cumple con el oráculo 'o'.
      val resultados = secuencias.filter(buscarEnSecuencia).seq
      resultados.find(_.nonEmpty).getOrElse(Seq.empty[Char]) // Retorna la primera secuencia válida encontrada o una secuencia vacía si no hay coincidencias.
    }
  }


  // implementación de la solución turbo paralela
  /**
   * Función que reconstruye una cadena usando un enfoque turbo acelerado de manera paralela si supera un umbral.
   *
   * @param umbral El umbral que define cuándo se debe realizar la reconstrucción de manera secuencial.
   * @param n Longitud deseada de la cadena a reconstruir.
   * @param o Oráculo utilizado para validar las subcadenas.
   * @return La cadena reconstruida que cumpla con la condición del oráculo.
   */
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

  // implementación de la solución turbo acelerada paralela
  /**
   * Función que reconstruye una cadena usando un enfoque turbo acelerado de manera paralela.
   *
   * @param umbral Límite para decidir si se aplica la reconstrucción de manera paralela.
   * @param n      Longitud deseada de la cadena a reconstruir.
   * @param o      Oráculo utilizado para validar las subcadenas.
   * @return La cadena reconstruida que cumpla con la condición del oráculo.
   */
  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    if (n < 1) {
      Seq.empty[Char] // Si n es menor a 1, retorna una secuencia vacía.
    } else {
      @tailrec
      def generarSecuenciasValidas(secuencias: ParSeq[Seq[Char]], arbol: ArbolSufijos.Trie, aumentador: Int): Seq[Char] = {
        // Verifica si alguna de las secuencias alcanza la longitud deseada 'n'.
        if (secuencias.exists(_.length == n)) {
          secuencias.find(_.length == n).getOrElse(Seq.empty[Char]) // Retorna la secuencia de longitud 'n' si se encuentra.
        } else {
          // Crea un nuevo árbol con las secuencias actuales y verifica combinaciones para generar nuevas secuencias válidas.
          val nuevoArbol = secuencias.foldLeft(arbol) { (arbolActual, secuencia) =>
            ArbolSufijos.adicionar(secuencia, arbolActual)
          }
          // Genera nuevas secuencias que cumplen con las restricciones del oráculo 'o' en paralelo.
          val nuevasSecuencias = (for {
            subSeq1 <- secuencias
            subSeq2 <- secuencias if subSeq1.length + subSeq2.length - 1 <= n
            valor = subSeq1 ++ subSeq2
            if (o(valor))
          } yield valor).par
          // Llama recursivamente para generar más secuencias válidas.
          generarSecuenciasValidas(nuevasSecuencias, nuevoArbol, aumentador * 2)
        }
      }

      // Si n es menor o igual al umbral, llama a la función secuencial.
      if (n <= umbral) {
        ReconstruirCadenas.reconstruirCadenaTurboAcelerada(n, o)
      } else {
        // Genera una secuencia inicial filtrando el alfabeto con el oráculo y la convierte en ParSeq.
        val secuenciaInicial = alfabeto.filter(c => o(Seq(c))).map(Seq(_)).to(ParSeq)
        // Crea el árbol de sufijos basado en la secuencia inicial.
        val arbolInicial = ArbolSufijos.arbolDeSufijos(secuenciaInicial.seq)
        // Inicia la generación de secuencias válidas de forma paralela.
        generarSecuenciasValidas(secuenciaInicial, arbolInicial, 1)
      }
    }
  }

  
}
