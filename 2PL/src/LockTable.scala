import java.util.concurrent.Semaphore
import scala.collection.mutable.HashMap

class LockTable {
	class Lock(val tid: Int, val shared: Boolean=false){
	  val sem = new Semaphore (0)
	  
	  override def toString: String = {
		synchronized {
			"Lock ( " + tid + " " + shared + ")"
	    }
	  }
	} //lock class
	
	var locks = new HashMap [Int, List[Lock]] //Int: oid, 
// HashMap is a very compact version of Array, cuz we can drop most unlock objects

	def S(tid: Int, oid: Int):Boolean = {
	  var lock: Lock = null
	  var wait = false
	  
	  synchronized {
			if( locks contains oid ){
				//wait = true
			  for(lock <- locks(oid))
				if(lock.shared == false){
				  wait = true
				}
			} // if
		} // synchronized
		
		if(wait) lock.sem.acquire() // thread waits until lock is released (unlock)
		
		synchronized {
		  if(locks contains oid)
		  {locks(oid) ++= List(new Lock(tid, true))}
		  else{locks += oid -> List(new Lock (tid, true))}		
		}
		true
	}
	
	def X(tid: Int, oid: Int):Boolean ={
		var lock: Lock = null
		var waits = List[Boolean]()

		synchronized {
			if( locks contains oid ){
				for(lock <- locks (oid)){
				  waits ++= List(true)
				}
			} // if
		} // synchronized
		//println(waits)
		
		 for(i <- 0 until waits.size; lock <- locks(oid)) {
		   var wait = waits(i)
		   if(wait) {println("wait");lock.sem.acquire()}
		 }// thread waits until lock is released (unlock)
		
		synchronized {
		  if(locks contains oid){locks(oid) :+ new Lock(tid)}
		  else{locks += oid -> List(new Lock (tid))}
		}
		true
	} // def X
	
	def U(tid: Int, oid: Int): Boolean ={
	  var lock: Lock = null
	  var error = false
	  
	  synchronized {
	    if(locks contains oid){
	      for(lock <- locks(oid)){
	    	  lock.sem.release()
	      }
	      locks -= oid
	    }
	    else{
	      error = true
	    }
	  }
	  true
	} // def U
	
    
    def releaseAll(t: LockTable){
      for((oid,lockList) <- t.locks){
        for(lock <- lockList)
        	t.U(lock.tid, oid)
      }
    }

	
	override def toString: String = {
		synchronized {
	    "LockTable ( " + locks + ")"
	  }
	}
}

