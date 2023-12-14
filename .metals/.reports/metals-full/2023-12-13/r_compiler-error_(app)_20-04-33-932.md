file:///C:/Users/Alex/Desktop/Proyecto-Final-PFC-main/app/src/main/scala/ReconstruirCadenas/ReconstruirCadenas.scala
### scala.reflect.internal.FatalError: no context found for source-file:///C:/Users/Alex/Desktop/Proyecto-Final-PFC-main/app/src/main/scala/ReconstruirCadenas/ReconstruirCadenas.scala,line-2,offset=11

occurred in the presentation compiler.

action parameters:
offset: 11
uri: file:///C:/Users/Alex/Desktop/Proyecto-Final-PFC-main/app/src/main/scala/ReconstruirCadenas/ReconstruirCadenas.scala
text:
```scala
/**
  * Pro@@yecto final - Programación Funcional y concurrente
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

  def secAlAzar(long: Int, s: Seq[Char]): Seq[Char] = {
    //Crea una secuencia de long caracteres del alfabeto,
    // escogidos de forma aleatoria, terminando en s
    val random = new Random()
    if (s.length == long) s
    else {
      val indiceAzar = random.nextInt(4)
      secAlAzar(long, alfabeto(indiceAzar) +: s)
    }
  }

  def PRC_Ingenuo(n: Int, o: Oraculo): Seq[Char] = {
    def generarSecuencias(n: Int, s: Seq[Char]): LazyList[Seq[Char]] = {
     if (n == 0) LazyList(s)
      else alfabeto.to(LazyList).flatMap(char => generarSecuencias(n - 1, s :+ char))
    }

    val resultado = generarSecuencias(n, Seq.empty).find(o)
    resultado.getOrElse(Seq.empty)
  }

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
      PRC_Ingenuo(n, o)
    } else {
      val resultados = secuencias.filter(buscarEnSecuencia).seq
      resultados.find(_.nonEmpty).getOrElse(Seq.empty[Char])
    }
  }


  def main(args: Array[String]): Unit = {
    println(saludo())
    println("Comparación de rendimiento entre PRC_Ingenuo y PRC_IngenuoPar")
    val resultados1 = for {
      i <- 1 to 8
       oraculo = Oraculo.crearOraculo(alfabeto)
    } yield (compararAlgoritmos(PRC_Ingenuo, PRC_IngenuoPar(6))(i, oraculo), "Tamano: " + i)
    resultados1.foreach(println)

    

  }
 }

```



#### Error stacktrace:

```
scala.tools.nsc.interactive.CompilerControl.$anonfun$doLocateContext$1(CompilerControl.scala:100)
	scala.tools.nsc.interactive.CompilerControl.doLocateContext(CompilerControl.scala:100)
	scala.tools.nsc.interactive.CompilerControl.doLocateContext$(CompilerControl.scala:99)
	scala.tools.nsc.interactive.Global.doLocateContext(Global.scala:114)
	scala.meta.internal.pc.PcDefinitionProvider.definitionTypedTreeAt(PcDefinitionProvider.scala:151)
	scala.meta.internal.pc.PcDefinitionProvider.definition(PcDefinitionProvider.scala:68)
	scala.meta.internal.pc.PcDefinitionProvider.definition(PcDefinitionProvider.scala:16)
	scala.meta.internal.pc.ScalaPresentationCompiler.$anonfun$definition$1(ScalaPresentationCompiler.scala:321)
```
#### Short summary: 

scala.reflect.internal.FatalError: no context found for source-file:///C:/Users/Alex/Desktop/Proyecto-Final-PFC-main/app/src/main/scala/ReconstruirCadenas/ReconstruirCadenas.scala,line-2,offset=11