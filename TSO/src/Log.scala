class Log  extends Thread with ReadWrite{

  var log = List[Tuple5[Char, Int, Int, Double, Double]]()
  //log tuple : operation, tid, oid, old value, new value
  def save(l:List[Tuple5[Char, Int, Int, Double, Double]]){
    log ++= l
  }
  
  def print(){
    println(log)
  }
}

object Log extends App{
  var l = new Log()
  //println(l)
  var temp = List(Tuple5('r', 1, 2, 3., 4.))
   l.save(temp)
  //println(temp)
  temp = List(Tuple5('w', 1, 2, 3., 4.))
  l.save(temp)
  l.print();
  
}