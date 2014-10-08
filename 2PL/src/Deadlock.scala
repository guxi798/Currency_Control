class Deadlock extends Thread with ReadWrite{
	var l = List[Tuple3[Char, Int, Int]]()
	
	def update(t: List[Tuple3[Char, Int, Int]]): Boolean = {
	  synchronized{
		  l ++= t
	  }
	  var s = new Schedule(l)
	  if(s.isCSR){
	    true
	  }
	  false
	}
	
	  def print(){
    println(l)
  }
	
}

