
//Simpson Simple

def integracion( f:Double=>Double, a:Double, b:Double):Double = {
    (b-a)*((f(a)+4*f((a+b)/2)+f(b))/6)
} 

val ejem1 = integracion(x => (-math.pow(x,2) + 8*x  - 12), 3, 5)
val ejem2 = integracion(x => 3*(math.pow(x,2)), 0, 2)
val ejem3 = integracion(x => x+2*(math.pow(x,2))+(math.pow(x,3))+5*(math.pow(x,4)),1,-1)
val ejem4 = integracion(x => (2*x+1)/((math.pow(x,2))+x) ,1,2) 
val ejem5 = integracion(x => math.pow((math.E),x) ,0, 1)
val ejem6 = integracion(x => 1 / math.sqrt(x-1),2,3)
val ejem7 = integracion(x => 1 / 1 + math.pow(x,2),0,1)

// Simpson Compuesta

def Compuesta(a : Int, b: Int, num : Int ,f :Double=> Double):Double = {
    val h = ( b - a )/num.toDouble 
    val xj = ( j : Double) => (a + j*h)

    val funciones = (j : Double) => f(xj(2 * j - 2)) + (4 * f(xj(2 * j - 1))) + (f(xj(
        2 * j)))

    ((1 to num/2).map(funciones(_)).sum)* h/3 
}

// Simpson Extendida

def extendida(f : Double => Double , a : Int, b : Int ) : Double = {
    
    val num = 2 * (b -a) 
    val h = ( b - a )/num.toDouble 
    val x = ( n : Double) => f(a + n*h)

    val funciones = f(a) + 4 * (1 to num-1 by 2).map(x(_)).sum + 2 *(2 to num-2 by 2).map(x(_)).sum +f(b)
    h/3 * funciones
}














