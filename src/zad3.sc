def derive(f: Double => Double, x:Double):Double = {
  (f(x+ 10e-7) - f(x) )/10e-7
}

def deriveDouble (f: Double => Double, g: Double => Double) : Double => Double = {
  (x) => derive(g, derive(f, x))
}



deriveDouble(x=>x*x + 1, y=>2*y*y)(2);