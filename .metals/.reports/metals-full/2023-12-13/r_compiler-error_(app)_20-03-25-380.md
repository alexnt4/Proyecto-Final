file:///C:/Users/Alex/Desktop/Proyecto-Final-PFC-main/app/src/main/scala/ArbolSufijos/package.scala
### scala.reflect.internal.FatalError: no context found for source-file:///C:/Users/Alex/Desktop/Proyecto-Final-PFC-main/app/src/main/scala/ArbolSufijos/package.scala,line-10,offset=156

occurred in the presentation compiler.

action parameters:
offset: 156
uri: file:///C:/Users/Alex/Desktop/Proyecto-Final-PFC-main/app/src/main/scala/ArbolSufijos/package.scala
text:
```scala
/*
Proyecto Final:

Alex García Castañeda <202259517>
Stiven Henao Aricapa <202259603>
Juan Sebastian Gómez Agudelo <202259474>

07/12/2023

Archivo: packag@@e.scala (ArbolSufijos)

*/

package object ArbolSufijos {

  // Definiendo otra estructura para manipular Seq[Seq[Char]]
  abstract class Trie

  case class Nodo(car: Char, marcada: Boolean,
                  hijos: List[Trie]) extends Trie

  case class Hoja(car: Char, marcada: Boolean) extends Trie

  def raiz(t: Trie): Char = {
    t match {
      case Nodo(c, _, _) => c
      case Hoja(c, _) => c
    }
  }

  def cabezas(t: Trie): Seq[Char] = {
    t match {
      case Nodo(_, _, lt) => lt.map(t => raiz(t))
      case Hoja(c, _) => Seq[Char](c)
    }
  }



  def pertenece(subSecuencia: Seq[Char], trie: Trie): Boolean = {
    //Recibe una secuencia s y un trie t y devuelve un booleano que responde si s ∈ t.

    // Función auxiliar que verifica si una subsecuencia está presente en la lista de cabezas de un trie.
    def perteneceEnCabezas(subSecuencia: Seq[Char], listaCabezas: List[Trie]): Boolean = {
      subSecuencia match {
        case cabeza +: cola =>
          listaCabezas.exists { subArbol =>
            raiz(subArbol) == cabeza && (pertenece(cola, subArbol) || pertenece(subSecuencia, subArbol))
          }
        case _ => true // La subsecuencia está vacía, es válida.
      }
    }

    trie match {
      case Nodo(_, _, hijos) => perteneceEnCabezas(subSecuencia, hijos)
      case Hoja(_, _) => subSecuencia.isEmpty
    }
  }



  def adicionar(s: Seq[Char], t: Trie): Trie = {
    //Recibe una secuencia s y un trie t y devuelve el trie correspondiente a adicionar s a t.

    // Genera la estructura correspondiente a la secuencia o al resto de la secuencia a añadir.
    def generarEstructura(s: Seq[Char]): Trie = {
      // Genera la estructura correspondiente a la secuencia.
      if (s.isEmpty) {
        Nodo(' ', marcada = false, List())
      } else {
        val cabeza = s.head
        val cola = s.tail
        // Crea un nodo marcado al final de la secuencia o un nodo no marcado en el resto de la secuencia.
        if (cola.isEmpty) {
          Hoja(cabeza, marcada = true)
        } else {
          Nodo(cabeza, marcada = false, List(generarEstructura(cola)))
        }
      }
    }


    def agregarRama(arbolActual: Trie, remaining: Seq[Char]): Trie = {
      (arbolActual, remaining) match {
        // Caso 1: Si el nodo actual tiene hijos y la cabeza de la secuencia está en las cabezas de los hijos.
        case (Nodo(car, marcada, hijos), head :: tail) if cabezas(Nodo(car, marcada, hijos)).contains(head) =>
          // Recorre recursivamente el árbol hasta llegar al camino deseado.
          val updatedHijos = hijos.map { hijo =>
            if (raiz(hijo) == head) agregarRama(hijo, tail)
            else hijo
          }
          // Retorna el nodo actualizado con los hijos actualizados.
          Nodo(car, marcada, updatedHijos)

        // Caso 2: Si el nodo actual es una hoja.
        case (Hoja(car, marcada), _) =>
          // Convierte la hoja en un Nodo con el nuevo "subárbol" como hijo.
          Nodo(car, marcada = true, List(generarEstructura(remaining)))

        // Caso 3: Si el nodo actual es un Nodo.
        case (Nodo(car, marcada, hijos), _) =>
          // Agrega el nuevo nodo a la lista de hijos cuando el camino se detiene en un Nodo.
          Nodo(car, marcada, hijos :+ generarEstructura(remaining))

        // Caso 4: Si el nodo actual es una hoja no marcada y no hay más elementos en la secuencia.
        case (Nodo(car, false, hijos), _) =>
          // Modifica el valor de marcada a true si no hay camino por recorrer pero los elementos de la cadena están en el árbol.
          Nodo(car, marcada = true, hijos)

        // Caso 5: En otros casos, devuelve el trie actual sin cambios.
        case (_, _) =>
          arbolActual
      }
    }

    agregarRama(t, s)
  }



  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    //Recibe una secuencia de secuencias ss y devuelve el trie correspondiente al
    //arbol de sufijos de ss
    val arbolVacio: Trie = Nodo(' ', marcada = false, List())

    def agregarSufijoAlArbol(arbol: Trie, sufijo: Seq[Char]): Trie = {
      // Utiliza la función adicionar para agregar el sufijo al árbol.
      adicionar(sufijo, arbol)
    }
    // Utiliza foldLeft para agregar cada secuencia al árbol de sufijos.
    ss.foldLeft(arbolVacio)(agregarSufijoAlArbol)
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

scala.reflect.internal.FatalError: no context found for source-file:///C:/Users/Alex/Desktop/Proyecto-Final-PFC-main/app/src/main/scala/ArbolSufijos/package.scala,line-10,offset=156