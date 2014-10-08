class Schedule (s:List[Tuple3[Char, Int, Int]]){

  def isCSR (nTrans: Int): Boolean ={
    
    val graph = new Array[Set[Int]](nTrans);
    val color = new Array [Char] (nTrans);
    
     for (i <- 0 until nTrans) { graph(i) = Set [Int] (); color(i) = 'G' } 
     
     
    for (si <- 0 until s.length-1)
    { 
      val i= s(si)
      for(sj <-1 until s.length)
      {
        val j=s(sj)
        // at least one write, different transaction, same object
        if((i._1== 'w' ||j._1 =='w') && (i._2!=j._2) && (i._3 == j._3))
        {
          //add edge
          graph(i._2-1) += (j._2-1) 
        }//if
      }//for j
      
    }//for i
    
    val g = new Graph(graph)
        
    return !(new Cycle(g).hasCycle)
    } // isCSR
  
  override def toString: String =
    {
        "Schedule ( " + s + " )"
    } //toString
  
  def foreach [U] (f: Tuple3 [Char, Int, Int] => U)
    {
        s.foreach (f)
    } //foreach
}

