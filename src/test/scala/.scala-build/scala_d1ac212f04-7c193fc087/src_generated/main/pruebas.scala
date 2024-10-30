

final class pruebas$_ {
def args = pruebas_sc.args$
def scriptPath = """src\test\scala\pruebas.sc"""
/*<script>*/
import Benchmark._
import kmedianas2D._




val puntos16_3 = generarPuntos(3, 16).toSeq
tiemposKmedianas(puntos16_3, 3, 0.01)
probarKmedianas(puntos16_3, 3, 0.01)


val puntos32768_256 = generarPuntos (256, 32768).toSeq
tiemposKmedianas (puntos32768_256, 256, 0.01 )


/*</script>*/ /*<generated>*//*</generated>*/
}

object pruebas_sc {
  private var args$opt0 = Option.empty[Array[String]]
  def args$set(args: Array[String]): Unit = {
    args$opt0 = Some(args)
  }
  def args$opt: Option[Array[String]] = args$opt0
  def args$: Array[String] = args$opt.getOrElse {
    sys.error("No arguments passed to this script")
  }

  lazy val script = new pruebas$_

  def main(args: Array[String]): Unit = {
    args$set(args)
    val _ = script.hashCode() // hashCode to clear scalac warning about pure expression in statement position
  }
}

export pruebas_sc.script as `pruebas`

