class Cycle (g: Graph) {
  
	private val color = Array.ofDim[Char](g.adj.length)
	
	for(i <- color.indices) color(i) = 'G'
	  
	def hasCycle(): Boolean=
	{
	  for(i <- color.indices if color(i) == 'G' && loopback(i)) return true
	  false
	}//hasCycle
	
	def loopback(i: Int): Boolean=
	{
	  if (color(i) == 'Y') return true
	  color(i) = 'Y'
	  for(j <- g.adj(i) if color(j) != 'R' && loopback(j)) return true
	  color(i) = 'R'
	  false
	}//loopback
}