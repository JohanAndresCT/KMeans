import Benchmark._
import kmedianas2D._

// Pruebas con tamaños en potencias de 2.
/** 16 puntos, 3 clusters **/
val puntos16_3 = generarPuntos(3, 16).toSeq //
tiemposKmedianas(puntos16_3, 3, 0.01)
probarKmedianas(puntos16_3, 3, 0.01)

/** 256 puntos, 10 clusters **/
val puntos256_10 = generarPuntos (10, 256).toSeq
tiemposKmedianas(puntos256_10, 10, 0.01)
probarKmedianas(puntos256_10, 10, 0.01)


/**65536 puntos, 256 clusters **/
val puntos65536_256 = generarPuntos (256, 65536).toSeq
tiemposKmedianas (puntos65536_256, 256, 0.01 )
probarKmedianas(puntos65536_256, 256, 0.01 )


