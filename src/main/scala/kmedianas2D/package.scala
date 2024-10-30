import scala.collection.parallel.CollectionConverters._
import scala.annotation.tailrec
import scala.util.Random
import common._
package object kmedianas2D {
  /** Clase Punto * */
  class Punto(val x: Double, val y: Double) {
    private def cuadrado(v: Double): Double = v * v

    def distanciaAlCuadrado(that: Punto): Double =
      cuadrado(that.x - x) + cuadrado(that.y - y)

    private def round(v: Double): Double = (v * 100).toInt / 100.0

    override def toString = s"(${round(x)}, ${round(y)})"
  }

  /** función hallarPuntoMasCercano * */
  def hallarPuntoMasCercano(p: Punto, medianas: Seq[Punto]): Punto = {
    assert(medianas.nonEmpty)
    medianas
      .map(pto => (pto, p.distanciaAlCuadrado(pto)))
      .sortWith((a, b) => a._2 < b._2)
      .head._1
  }

  /** función hallarPuntoMasCercano * */
  def calculePromedioSeq(medianaVieja: Punto, puntos: Seq[Punto]): Punto = {
    if (puntos.isEmpty) medianaVieja
    else {
      new Punto(puntos.map(p => p.x).sum / puntos.length, puntos.map(p => p.y).sum / puntos.length)
    }
  }

  def calculePromedioPar(medianaVieja: Punto, puntos: Seq[Punto]): Punto = {
    if (puntos.isEmpty) medianaVieja
    else {
      val puntosPar = puntos.par
      new Punto(puntosPar.map(p => p.x).sum / puntos.length, puntosPar.map(p => p.y).sum / puntos.length)
    }
  }

  /** 1.1. Clasificando los puntos * */
  // Implementación secuencial de la clasificación
  def clasificarSeq(puntos: Seq[Punto], medianas: Seq[Punto]): Map[Punto, Seq[Punto]] = {
    puntos.groupBy(p => hallarPuntoMasCercano(p, medianas))
  }

  // Implementación concurrente de la clasificación
  def clasificarPar(umb: Int)(puntos: Seq[Punto], medianas: Seq[Punto]): Map[Punto, Seq[Punto]] = {
    if (puntos.size > umb) {
      val (left, right) = puntos.splitAt(puntos.size / 2)
      val leftMap = clasificarPar(umb)(left, medianas)
      val rightMap = clasificarPar(umb)(right, medianas)
      (leftMap ++ rightMap).groupBy(_._1).map {
        case (k, v) => k -> v.flatMap(_._2).toSeq
      }
    } else {
      clasificarSeq(puntos, medianas)
    }
  }

  /** 1.2. Actualizando las medianas * */
  // Función de actualización secuencial
  def actualizarSeq(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]): Seq[Punto] = {
    for{
      mediana <- medianasViejas
      medianaActualizada = calculePromedioSeq(mediana, clasif.getOrElse(mediana, Seq()))
    } yield medianaActualizada
  }

  // Función de actualización concurrente
  def actualizarPar(clasif: Map[Punto, Seq[Punto]], medianasViejas: Seq[Punto]): Seq[Punto] = {
    medianasViejas.par.map(mediana => calculePromedioPar(mediana, clasif.getOrElse(mediana, Seq()))).toList
  }

  /** 1.3. Detectando convergencia * */
  def converge(eta: Double, medianasViejas: Seq[Punto], medianasNuevas: Seq[Punto]) : Boolean = {
    medianasViejas.zip(medianasNuevas).forall { case (vieja, nueva) =>
      vieja.distanciaAlCuadrado(nueva) <= eta
    }
  }
  def hayConvergenciaSeq(eta: Double, medianasViejas: Seq[Punto], medianasNuevas: Seq[Punto]): Boolean = {
    var convergencia = true
    for (i <- medianasViejas.indices if convergencia) {
      val distanciaCuadrada = medianasViejas(i).distanciaAlCuadrado(medianasNuevas(i))
      if (distanciaCuadrada >= eta) convergencia = false
    }
    convergencia
  }
  def hayConvergenciaPar(eta: Double, medianasViejas: Seq[Punto], medianasNuevas: Seq[Punto]): Boolean = {
    val (medianasViejas1, medianasViejas2) = medianasViejas.splitAt(medianasViejas.size / 2)
    val (medianasNuevas1, medianasNuevas2) = medianasNuevas.splitAt(medianasNuevas.size / 2)
    val (convergencia1, convergencia2) = parallel(
      converge(eta, medianasViejas1, medianasNuevas1),
      converge(eta, medianasViejas2, medianasNuevas2))
    convergencia1 && convergencia2
  }

  /** 1.4. Implementando el algoritmo kmeans **/
  // Implementación del algoritmo KMeans en su versión secuencial
  @tailrec
  final def kMedianasSeq(puntos: Seq[Punto], medianas: Seq[Punto], eta: Double): Seq[Punto] = {
    val clasif = clasificarSeq(puntos, medianas)
    val medianasActualizadas = actualizarSeq(clasif, medianas)
    if (hayConvergenciaSeq(eta, medianas, medianasActualizadas)) {
      medianasActualizadas // Retorna las medianas si se ha alcanzado la convergencia
    } else {
      // Llamada recursiva para la siguiente iteración si no ha convergido
      kMedianasSeq(puntos, medianasActualizadas, eta)
    }
  }
  @tailrec
  final def kMedianasPar(puntos: Seq[Punto], medianas: Seq[Punto], eta: Double): Seq[Punto] = {
    val clasificacion = clasificarPar(10)(puntos, medianas)
    val medianasNuevas = actualizarPar(clasificacion, medianas)
    if (hayConvergenciaPar(eta, medianas, medianasNuevas)) medianasNuevas
    else kMedianasPar(puntos, medianasNuevas, eta)
  }

  /** 1.5. Corriendo el algoritmo **/
  // Función para generar puntos aleatorios en el espacio 2D
  def generarPuntos(k: Int, num: Int): Seq[Punto] = {
    val randx = new Random(1)
    val randy = new Random(3)
    (0 until num).map { i =>
      val x = ((i + 1) % k) * 1.0 / k + randx.nextDouble() * 0.5
      val y = ((i + 5) % k) * 1.0 / k + randy.nextDouble() * 0.5
      new Punto(x, y)
    }
  }

  // Función para inicializar las medianas seleccionando k puntos aleatorios
  def inicializarMedianas(k: Int, puntos: Seq[Punto]): Seq[Punto] = {
    val rand = new Random(7)
    (0 until k).map(_ => puntos(rand.nextInt(puntos.length)))
  }

}


