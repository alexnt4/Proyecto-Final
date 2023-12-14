/*
Proyecto Final:

Alex García Castañeda <202259517>
Stiven Henao Aricapa <202259603>
Juan Sebastian Gómez Agudelo <202259474>

*/

package object Oraculo {
  type Oraculo = Seq[Char] => Boolean

  def crearOraculo(c: Seq[Char]): Oraculo = {
    def esSubcadena(s: Seq[Char]): Boolean = {
      c.containsSlice(s)
    }

    esSubcadena
  }
}
