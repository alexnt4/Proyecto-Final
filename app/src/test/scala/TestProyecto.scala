/*
Proyecto Final:

Alex García Castañeda <202259517> 
Stiven Henao Aricapa <202259603>
Juan Sebastian Gómez Agudelo <202259474>

*/
import ReconstruirCadenas.ReconstruirCadenas
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestProyecto extends AnyFunSuite{
    test("testTaller4"){
        assert("Proyecto Final 2023-II" == ReconstruirCadenas.saludo())
    }

    // test222233
    
    test("test turbo y turbo paralela") {
        val resultados1 = for {
            i <- 1 to 4
            size = math.pow(2, i).toInt
            // Creación del oráculo y la cadena objetivo
            cadenaObjetivo = secAlAzar(size, Seq.empty)
            oraculo = Oraculo.crearOraculo(cadenaObjetivo)
            // Resultados de solucion turbo y turbo paralela
            resultadoTurbo = reconstruirCadenaTurbo(size, oraculo)
            resultadoTurboPar = reconstruirCadenaTurboPar(4)(size, oraculo)
        } yield {
            println(s"Cadena objetivo para tamano $size: $cadenaObjetivo")
            // Comparación con la cadena objetivo
            val coinciden = resultadoTurbo == cadenaObjetivo && resultadoTurboPar == cadenaObjetivo
            println(s"Resultados coinciden para tamano $size: $coinciden")
            coinciden // retorna el resultado de la comparación
        }

        // verificar que todas las coincidencia sean verdaderas
        assert(resultados1.forall(coinciden => coinciden), "No todas las coincidencias fueron verdaderas")
    }
    
}
