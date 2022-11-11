import scala.annotation.tailrec

def findMinMax(list:List[List[Double]]):List[(Double,Double)] = {
  if list.isEmpty then List()
  def findMinMax_rec(xss:List[List[Double]], res:List[(Double,Double)]):List[(Double,Double)] = {
    if xss.isEmpty then return res
    else if xss.head.isEmpty then throw Error(s"brak min max w pustej liscie")

    @tailrec
    def findMinMax_iter(xs:List[Double], min:Double, max:Double):(Double,Double)={
      if xs.isEmpty then (min, max)
      else {
        val comp = xs.head
        comp match {
          case comp if comp < min => findMinMax_iter(xs.tail, comp, max)
          case comp if comp > max => findMinMax_iter(xs.tail, min, comp)
          case _ => findMinMax_iter(xs.tail, min, max)
        }
      }

    }
    findMinMax_rec(xss.tail, findMinMax_iter(xss.head, xss.head.head, xss.head.head)::res)
  }
  findMinMax_rec(list, Nil).reverse
}

//findMinMax(List(List(1,5,9,2,3), List(9,9,9,8,3), List(), List(5,4,6)));
//findMinMax(List(List(1,5,9,2,3), List(9,9,9,8,3), List(1), List(5,4,6)));

def findMinMax_fold(list:List[List[Double]]):List[(Double,Double)] = {
  list.map((element: List[Double]) =>
    element.foldLeft((element.head, element.head))((prev: (Double, Double), head: Double) => {
      if head < prev._1 then (head, prev._2)
      else if head > prev._2 then (prev._1, head)
      else prev
    }
    )
  )
}

//findMinMax_fold(List(List(1,5,9,2,3), List(9,9,9,8,3), List(), List(5,4,6)));
findMinMax_fold(List(List(1,5,9,2,3), List(9,9,9,8,3), List(1), List(5,4,6)));


//List(1, 2, 3, 4, 5) foldLeft((0,0))()