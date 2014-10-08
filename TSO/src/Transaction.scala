trait ReadWrite
{
    val r = 'r'
    val w = 'w'

} // ReadWrite

object Database 
{
    /** The lock table to control concurrent access to the database
     */
    //val lt   = new LockTable ()

    /** The abstract database representation (an array of double objects)
     */
    val data = new Array [Double] (100)

} // Database object


object Transaction extends App{
println ("Test Transactions")
    val s1 = new Schedule (List ( ('r', 1, 0), ('w', 1, 1) ))
    val s2 = new Schedule (List ( ('w', 2, 0), ('w', 2, 1) ))
    var tso = new TimeStamp();
	var l = new Log();
	//log tuple : operation, tid, oid, old value, new value
	//var log = List[Tuple5[Char, Int, Int, Double, Double]]()
    //var ts:Int = 0;
    val t1 = new Transaction (1, s1, tso.setts())
    //ts+=1
    val t2 = new Transaction (2, s2, tso.setts())

   
    t1.start ()
    t2.start ()
    
    //println("log "+ l.print())
 class Transaction (tid: Int, s:Schedule, ts:Int) extends Thread with ReadWrite{
   //var RTS, WTS :Int = 0;
  //println(tid + ts)
   //var RTS = tso.getRTS();
   //var WTS = tso.getWTS();
      //var log = List[Tuple5[Char, Int, Int, Double, Double]]()
  override def run(){
    var value = 0.
    begin()
    for(op <- s){
    	//perform reads/writes:get locks->unlock
      if (op._1 == 'r') {
        value = read (op._3)
            } else {
               write (op._3, value + 1.)
            } // if
    }//for
    commit()
    
  }//run

  def begin ()
    {
        Thread.sleep (5)
        println ("begin transaction " + tid+ " timestamp is "+ ts)
        
    } // begin
  
  def read(oid:Int):Double={
   // val Record
  //return Record
    Thread.sleep (10)
    
    synchronized{
      println("Read operation, WTS= "+ tso.getWTS())
    if(tso.getWTS()>ts){
      println("Read operation, rollback "+tid)
      rollback(tid)
      return -1
    }
    else{
    val value = Database.data(oid) 
    tso.setRTS(ts)
    println("Read operation, setRTS= "+ tso.getRTS())
    //log tuple : operation, tid, oid, old value, new value
    l.save(List(Tuple5('r', tid, oid, value, value)));
    println ("read tid" + tid + " ( oid " + oid + " ) value = " + value)
    //println("log ")
    //l.print()
    println()
    value
  }
  }//synchronized
  }//read
  
  def write(oid:Int, value:Double):Double= {
    Thread.sleep (15)
    synchronized{
      println("Write operation, WTS= "+ tso.getWTS()+" RTS = "+tso.getRTS())
    if(tso.getRTS()>ts){
      rollback(tid)
      println("Write operation rollback tid "+ tid) 
      return -1
    }
    else {
      println ("write " + tid + " ( " + oid + " ) value = " + value)
      l.save(List(Tuple5('w', tid, oid, value, value)));
      Database.data(oid) = value
      tso.setWTS(ts)
      println("Write operation, setWTS= "+ tso.getWTS())
      println("log ")
      l.print()
    }
    }//synchronized
    return 1
  }//write
   
  def commit ()
    {
        Thread.sleep (20)
        println ("commit transaction " + tid)
    } // commit
  
  def rollback(tid:Int){
    //log tuple : operation, tid, oid, old value, new value
    for(log<-l.log)
    {
      if(log._2 == tid){
        Database.data(log._3) = log._4
      }// if log._2 == tid
    }//for
  }
}//class
}