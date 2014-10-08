class Schedule (l0: List [Tuple3 [Char, Int, Int]]) {
  val l = l0
  val len = l.length
	val nTrans = l.maxBy(_._2)._2
	
  def isCSR(): Boolean = {
	val graph = new Array[Set[Int]](nTrans)
	for(i <- 0 to nTrans-1) graph(i) = Set[Int]()
	
	for(i <- 0 to l.length-2;j <- i+1 to l.length-1){
	  //graph :+ Set[Int]() //initialize
	   if(!(l(i)._1 == 'r' && l(j)._1 == 'r') && (l(i)._2 != l(j)._2) && (l(i)._3 == l(j)._3)){
	     if(! graph(l(i)._2 - 1).seq(l(j)._2 - 1)) //if the edge hasn't be constructed
	    	 graph(l(i)._2 - 1) += l(j)._2 - 1 // second position stores transaction ID, point T(l(i)._2) to Set(l(j)._2
	  } //inner for
	} //outer for
	val g = new Graph(graph)
	if(new Cycle(g).hasCycle()) return false
	true
  }
	
	def foreach[U] (f: Tuple3[Char, Int, Int] => U){
	  l.foreach(f)
	}
}