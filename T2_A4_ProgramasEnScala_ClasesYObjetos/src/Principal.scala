import scala.io.StdIn._

class Paciente(nombre:String, primerAp:String,segundoAP:String, edad:Byte,
               val fecha:Array[String], val horaDeRegistro:Array[String],val nivelBienestar:Array[Byte], 
               val temperaturas:Array[Double],val humedad:Array[Double]) {
  
   def imprimir(): Unit = {
    println("Nombre: " + nombre)
    println("Primer Apellido: " + primerAp)
    println("Segundo Apellido: " + segundoAP)
    println("Edad: " + edad)
    println("Fechas: ")
    fecha.foreach(println)
    println("Hora de registro: ")
    horaDeRegistro.foreach(println)
    println("Nivel de bienestar: ")
    nivelBienestar.foreach(println)
    println("Temperaturas: ")
    temperaturas.foreach(println)
    println("Humedad: ")
    humedad.foreach(println)
  }
  
  def nivelPromBien(): Double ={
    var sum = 0
    for (i <- 0 until nivelBienestar.length) {
      sum += nivelBienestar(i)
    }
    (sum / nivelBienestar.length)
  }
  
  def tempMayor(): Unit={
    
    var mayor = 0
    for (i <- 0 until temperaturas.length) {
      if (temperaturas(i) > mayor) {
        mayor = i
      }
    }
    println("\n------------------------------------------------------")
    println("La temperatura mayor es: " + temperaturas(mayor)+" °C")
    println("fecha: " + fecha(mayor) + "\n Hora: " + horaDeRegistro(mayor))
    println("Nivel de bienestar: " + nivelBienestar(mayor))
    print("Humedad: " + humedad(mayor)+" %")
  }
  
  def tempMenor(): Unit={
    var menor = 0
    for (i <- 0 until temperaturas.length) {
      if (temperaturas(i) < menor) {
        menor = i
      }
    }
   println("\n---------------------------------------------------------")
    println("La temperatura menor es: " + temperaturas(menor)+" °C")
    println("fecha: " + fecha(menor) + "\n Hora: " + horaDeRegistro(menor))
    println("Nivel de bienestar: " + nivelBienestar(menor))
    print("Humedad: " + humedad(menor)+" %")
  }
  
}//classPaciente

object Principal {
  def llenarFechas(n: Int) = {
    var fechas = new Array[String](n)
    println("x "+fechas.length)
    for (i <- 0 to fechas.length-1) {
      var dia = 0; 
      val año = (1900 + math.random*( 2021 - 1900)).toInt;
      val mes = (1 + math.random*( 12 - 1)).toInt;
      
      mes match{
      case 1 => (dia = (1 + math.random*( 31 - 1)).toInt)
      case 2 => 
        val x =0;
        val h = 1900;
        for( h <- 1900 to 2021 by + 4){
        if(h.equals(año)){
          x==1;
          h==2021;
          
        }

      }
        if(x==0)(dia = (1 + math.random*( 28 - 1)).toInt)
        else(dia = (1 + math.random*( 29 - 1)).toInt)
      case 3 => (dia = (1 + math.random*( 31 - 1)).toInt)
      case 4 => (dia = (1 + math.random*( 31 - 1)).toInt)
      case 5 => (dia = (1 + math.random*( 31 - 1)).toInt)
      case 6 => (dia = (1 + math.random*( 31 - 1)).toInt)
      case 7 => (dia = (1 + math.random*( 31 - 1)).toInt)
      case 8 => (dia = (1 + math.random*( 31 - 1)).toInt)
      case 9 => (dia = (1 + math.random*( 31 - 1)).toInt)
      case 10 => (dia = (1 + math.random*( 31 - 1)).toInt)
      case 11 => (dia = (1 + math.random*( 31 - 1)).toInt)
      case 12 => (dia = (1 + math.random*( 31 - 1)).toInt)
      
      }
      fechas(i) = dia + "/" + mes + "/" + año
    }
    fechas
  }
  
  def llenarHoraRegistro(n: Int) = {
    var horaRegistro = new Array[String](n)
    for (i <- 0 until horaRegistro.length) {
      val hora = (0 + math.random*( 23 - 0)).toInt
      val min = (0 + math.random*( 59 - 0)).toInt
      horaRegistro(i) = hora + ":" + min
    }
    horaRegistro
  }
  
  
  def llenarNivelBienestar(n: Int) = {
    var nivelBienestar = new Array[Byte](n)
    for (i <- 0 until nivelBienestar.length) {
      nivelBienestar(i) = (1 + math.random*( 5 - 1)).toByte
    }
    nivelBienestar
  }
  
  def llenartemperaturas(n: Int) = {
    var temperaturas = new Array[Double](n)
    for (i <- 0 until temperaturas.length) {
      temperaturas(i) = (-20 + math.random*( 50 +20))
    }
    temperaturas
  }
  
    def llenarHumedad(n: Int) = {
    var humedad = new Array[Double](n)
    for (i <- 0 until humedad.length) {
      humedad(i) = (0 + math.random*( 100 - 0))
    }
    humedad
  }
    
    
   
  def main(args: Array[String]): Unit = {
    println("Ingrese el nombre: ")
    val nombre = readLine()
    println("Ingrese primer apellido: ")
    val primerAp = readLine()
    println("Ingrese segundo apellido: ")
    val segundoAp = readLine()
    println("Ingrese edad: ")
    val edad = readByte()
    println("Numero de mediciones: ")
    val n = readInt()
    var paciente = new Paciente(nombre, primerAp, segundoAp, edad, llenarFechas(n), llenarHoraRegistro(n), llenarNivelBienestar(n), llenartemperaturas(n), llenarHumedad(n));
    
    
    paciente.imprimir()
    println("El promedio de Bienestar es Nivel: "+paciente.nivelPromBien())
    paciente.tempMayor()
    println()
    paciente.tempMenor()
    println()
    
  }
}