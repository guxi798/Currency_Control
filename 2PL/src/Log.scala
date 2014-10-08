class Log  extends Thread with ReadWrite{

  var log = List[Tuple5[Char, Int, Int, Double, Double]]()
  //log tuple : operation, tid, oid, old value, new value
  def save(l:List[Tuple5[Char, Int, Int, Double, Double]]){
    synchronized{
    	log ++= l
    }
  }
  
  def print(){
    println(log)
  }
}
