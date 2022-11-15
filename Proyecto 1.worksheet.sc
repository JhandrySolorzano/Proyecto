
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



// Simpson Extendida

def extendida(n : Double => Double , a : Int, b : Int ) : Double = {
    
    val h = ((b - a) / n)
    val n = 2 * (b - a)
    (h / 3) * ((n (a) + 4 * compro( f (a + compro)) + 2 f(a + (compro)* h ) + f (b)))
    
}

def compro( k : Int) : Int = (k % 2)match{
    case 0 => 1
    case 1 => 0
}









