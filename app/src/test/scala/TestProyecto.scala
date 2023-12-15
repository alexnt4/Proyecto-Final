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
import ReconstruirCadenas._
import ReconstruirCadenasPar._

@RunWith(classOf[JUnitRunner])
class TestProyecto extends AnyFunSuite{
    test("testProyectoFinal"){
        assert("Proyecto Final 2023-II" == ReconstruirCadenas.saludo())
    }

    test("Test ingenuo") {
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
            coinciden // Retornar el resultado de la comparación
        }

        // Verificar que todas las coincidencias sean verdaderas
        assert(resultados1.forall(coinciden => coinciden), "No todas las cadenas coinciden con la cadena objetivo")
    }

    test("Test Solucion Mejorada y Mejorada Par") {
        val resultados = for {
            i <- 1 to 8
            // Creación del oráculo y la cadena objetivo
            cadenaObjetivo = secAlAzar(i, Seq.empty)
            oraculo = Oraculo.crearOraculo(cadenaObjetivo)
            // Resultados de Solucion mejorada y su version paralelizada
            resultadoSolucionMejorada = ReconstruirCadenas.SolucionMejorada(i, oraculo)
            resultadoSolucionMejoradaPar = ReconstruirCadenasPar.SolucionMejoradaPar(4)(i, oraculo)
        } yield {
            println(s"Cadena objetivo para tamano $i: $cadenaObjetivo")
            // Comparación con la cadena objetivo
            val coinciden = resultadoSolucionMejorada == cadenaObjetivo && resultadoSolucionMejoradaPar == cadenaObjetivo
            println(s"Resultados coinciden para tamano $i: $coinciden")
            coinciden // Retornar el resultado de la comparación
        }
        println("\n\n")
        // Verificar que todas las coincidencias sean verdaderas
        assert(resultados.forall(coinciden => coinciden), "No todas las cadenas coinciden con la cadena objetivo")

    }


    test("test turbo y turbo paralela") {
        val resultados1 = for {
            i <- 1 to 4
            size = math.pow(2, i).toInt
            // Creación del oráculo y la cadena objetivo
            cadenaObjetivo = secAlAzar(size, Seq.empty)
            oraculo = Oraculo.crearOraculo(cadenaObjetivo)
            // Resultados de solucion turbo y turbo paralela
            resultadoTurbo = reconstruirCadenaTurbo(size, oraculo)
            resultadoTurboPar = ReconstruirCadenasPar.reconstruirCadenaTurboPar(4)(size, oraculo)
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

    test("Test Solucion Turbo Mejorada y Turbo Mejorada Par") {
        val resultados1 = for {
            i <- 1 to 8
            size = math.pow(2, i).toInt
            // Creación del oráculo y la cadena objetivo
            cadenaObjetivo = secAlAzar(size, Seq.empty)
            oraculo = Oraculo.crearOraculo(cadenaObjetivo)
            // Resultados de Turbo Mejorada y su version paralela
            resultadoTurboMejorada = ReconstruirCadenas.TurboMejorado(size, oraculo)
            resultadoTurboMejoradaPar = ReconstruirCadenasPar.TurboMejoradoPar(4)(size, oraculo)
        } yield {
            println(s"Cadena objetivo para tamano $size: $cadenaObjetivo")
            // Comparación con la cadena objetivo
            val coinciden = resultadoTurboMejorada == cadenaObjetivo && resultadoTurboMejoradaPar == cadenaObjetivo
            println(s"Resultados coinciden para tamano $size: $coinciden")
            coinciden // Retornar el resultado de la comparación
        }

        // Verificar que todas las coincidencias sean verdaderas
        assert(resultados1.forall(coinciden => coinciden), "No todas las cadenas coinciden con la cadena objetivo")
    }

    test("Test TurboAcelerada") {
        val resultados2 = for {
            i <- 1 to 8 // Cambiando el rango para representar potencias de 2 de 2 a 16
            size = math.pow(2, i).toInt // Calculando la potencia de 2
            // Creación del oráculo y la cadena objetivo
            cadenaObjetivo = secAlAzar(size, Seq.empty)
            oraculo = Oraculo.crearOraculo(cadenaObjetivo)
            // Resultados de PRC_TurboAcelerada y PRC_TurboAceleradaPar
            resultadoTurboAcelerada = reconstruirCadenaTurboAcelerada(size, oraculo)
            resultadoTurboAceleradaPar = ReconstruirCadenasPar.reconstruirCadenaTurboAceleradaPar(6)(size, oraculo)
        } yield {
            println(s"Cadena objetivo para tamaño $size: $cadenaObjetivo")
            // Comparación con la cadena objetivo
            val coinciden = resultadoTurboAcelerada == cadenaObjetivo && resultadoTurboAceleradaPar == cadenaObjetivo
            println(resultadoTurboAcelerada, resultadoTurboAceleradaPar)
            println(s"Resultados coinciden para tamaño $size: $coinciden")
            coinciden // Retornar el resultado de la comparación
        }

        // Verificar que todas las coincidencias sean verdaderas
        assert(resultados2.forall(coinciden => coinciden), "No todas las cadenas coinciden con la cadena objetivo")

    }
    
}
