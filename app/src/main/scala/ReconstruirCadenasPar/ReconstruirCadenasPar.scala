package ReconstruirCadenasPar

import org.scalameter.{Warmer, withWarmer}
import common._
import Benchmark._
import Oraculo._
import ReconstruirCadenas._

import scala.annotation.tailrec
import scala.collection.parallel.immutable.{ParSeq, ParSet}
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

  //Implementacion de la solucion mejorada paralela
  /**
   * Método encargado de reconstruir una cadena de manera mejorada de forma paralela si supera un umbral.
   *
   * @param umbral Valor límite para decidir si se aplica la reconstrucción de manera paralela.
   * @param n      Longitud de las subcadenas a generar.
   * @param o      Oráculo utilizado para validar las subcadenas.
   * @return La subcadena que cumpla con la condición del oráculo.
   */
  def SolucionMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    //Si n es menor que uno la secuencia vacia
    if (n < 1) {
      Seq.empty[Char]
    } else if (n <= umbral) { //Si es menor que el umbral la manda a la funcion sencuencial
      ReconstruirCadenas.SolucionMejorada(n, o) //Llamado a la funcion secuencial
    } else {
      def subcadenas(alfabeto: Seq[Char], longitud: Int): ParSeq[Seq[Char]] = {
        if (longitud == 0)
          ParSeq(Seq.empty[Char])
        else {
          val subcadenas_validas_anteriores = subcadenas(alfabeto, longitud - 1)
          //Misma implementacion pero con .par
          (for (
            un_caracter <- alfabeto;
            una_subcadena <- subcadenas_validas_anteriores;
            cadenas_candidatas = un_caracter +: una_subcadena
            if o(cadenas_candidatas)
          ) yield cadenas_candidatas).par
        }
      }

      subcadenas(alfabeto, n).headOption.getOrElse(Seq.empty[Char])
      /*
      Se usa el headOption para manejar el caso en que la
      secuencia es vacía y también se ha corregido la
      implementación de subcadenas en la función paralelizada
      para evitar posibles problemas de ordenamiento.
       */
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

  //Implementacion de la solucion turbo mejorada paralela
  /**
   * Método encargado de reconstruir una cadena de manera turbo mejorada de forma
   * paralela si supera un umbral.
   * @param umbral Valor límite para decidir si se aplica la reconstrucción de manera paralela.
   * @param n      Longitud de las subcadenas a generar.
   * @param o      Oráculo utilizado para validar las subcadenas.
   * @return La subcadena que cumpla con la condición del oráculo.
   */
  def TurboMejoradoPar(umbral: Int)(n: Int, PezOraculo: Oraculo): Seq[Char] = {
    if (n < 1) {
      Seq.empty[Char]
    }
    else if (n <= umbral) {
      //if para evaluar si n es menor y seria mejor hacerce por la secuencial
      ReconstruirCadenas.TurboMejorado(n, PezOraculo)
    }
    else {
      def verificarSecuencias(sec: Seq[Char], subsecuencias: ParSeq[Seq[Char]], l: Int): Boolean = {
        if (sec.length == l + 1) {
          true
        } else {
          val test = sec.slice(1, 1 + l)
          if (subsecuencias.seq.contains(test)) {
            verificarSecuencias(sec.drop(1), subsecuencias, l)
          } else {
            false
          }
        }
      }

      def subcadenasTurbo(alfabeto: Seq[Char], longitud: Int): ParSeq[Seq[Char]] = {
        if (longitud == 1)
          (for (un_caracter <- alfabeto; if (PezOraculo(Seq(un_caracter)))) yield Seq(un_caracter)).par
        else {
          val SubCadenasBuenasAnteriores = subcadenasTurbo(alfabeto, longitud / 2)
          (for (
            sub_cadena_1 <- SubCadenasBuenasAnteriores;
            sub_cadena_2 <- SubCadenasBuenasAnteriores;
            valores = sub_cadena_1 ++ sub_cadena_2
            if (verificarSecuencias(valores, SubCadenasBuenasAnteriores, longitud / 2))
            if PezOraculo(valores)
          ) yield valores).par

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
