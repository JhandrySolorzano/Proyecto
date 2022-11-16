
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

    val funciones = (j : Double) => f(xj(2 * j - 2)) + (4 * f(xj(2 * j - 1))) + (f(xj(2 * j)))

    ((1 to num/2).map(funciones(_)).sum)* h/3 
}

val c1 = ((x : Double) =>  (-math.pow(x,2) + 8*x  - 12))
Compuesta(3 , 5 , 70 ,c1) 

val c2 = ((x : Double) => 3*(math.pow(x,2)))
Compuesta(0 , 2 , 70 ,c2)

val c3 = (x : Double) => x+2*(math.pow(x,2))+(math.pow(x,3))+5*(math.pow(x,4))
Compuesta(1 , -1 , 70 ,c3)

val c4 = (x : Double) => (2*x+1)/((math.pow(x,2))+x)
Compuesta(1 , 2 , 70 ,c4)

val c5 = (x : Double) => math.pow((math.E),x)
Compuesta(0 , 1 , 70 ,c5)

val c6 = (x : Double) => 1 / math.sqrt(x-1)
Compuesta(2 , 3 , 70 ,c6)

val c7 = (x : Double) => 1 / 1 + math.pow(x,2)
Compuesta(0 , 1 , 70 ,c7)


// Simpson Extendida

def Extendida(a : Int, b: Int,f :Double=> Double ) : Double = {
    
    val num = 2 * (b -a) 
    val h = ( b - a )/num.toDouble 
    val x = ( n : Double) => f(a + n*h)

    val funciones = f(a) + 4 * (1 to num-1 by 2).map(x(_)).sum + 2 *(2 to num-2 by 2).map(x(_)).sum +f(b)
    h/3 * funciones
}

val ex1 = ((x : Double) =>  (-math.pow(x,2) + 8*x  - 12))
Extendida(3, 5, ex1)

val ex2 = ((x : Double) => 3*(math.pow(x,2)))
Extendida(0 , 2  ,ex2)

val ex3 = (x : Double) => x+2*(math.pow(x,2))+(math.pow(x,3))+5*(math.pow(x,4))
Extendida(1 , -1  ,ex3)

val ex4 = (x : Double) => (2*x+1)/((math.pow(x,2))+x)
Extendida(1 , 2  ,ex4)

val ex5 = (x : Double) => math.pow((math.E),x)
Extendida(0 , 1  ,ex5)

val ex6 = (x : Double) => 1 / math.sqrt(x-1)
Extendida(2 , 3  ,ex6)

val ex7 = (x : Double) => 1 / 1 + math.pow(x,2)
Extendida(0 , 1 ,ex7)













