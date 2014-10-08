import scala.util.control.Breaks._

object Database {
	    // The lock table to control concurrent access to the database
    val lt   = new LockTable ()

    // The abstract database representation (an array of double objects)

    var data = new Array[List[Double]](100)
    
}

/***********************************************************************************
 * Test the Transcation class by running several concurrent transactions/threads.
 */
object TransactionTest extends Application with Operations
{
    println ("Test Transactions")
    val s1 = new Schedule (List((r, 1, 0), (r, 1, 1), (w, 1, 0), (w, 1, 1)))
    val s2 = new Schedule (List((r, 2, 1), (w, 2, 1)))
    
    val t1 = new Transaction (1, s1)
    val t2 = new Transaction (2, s2)
    var d = new Deadlock()
    var log = new Log()

    t1.start ()
    t2.start ()
    
    class Transaction (tid: Int, s: Schedule) extends Thread
{
    /*******************************************************************************
     * Run the transaction: begin, reads/writes, commit.
     */
 
    override def run ()
    {
        var value = 0.
        var tag = 1
        
        begin ()
       breakable{
        for (i <- 0 until s.len) {
         var op = s.l(i)
         /** detect cycle in the current updated schedule
          */
          if(d.update(List(op))){
            println("Not in CSR")
            rollback(op._2)
            tag = 0
            break
          }
         /** No cycle detected, start getting locks
          */
          if (op._1 == r) {
        	  println("read")
              Database.lt.R (tid, op._3)
              value = read (op._3)
          }
          else if(op._1 == w){
              println("write")
              d.print()
              Database.lt.W (tid, op._3)
              write (op._3, value + 1.)
          }
          else if(op._1 == c){
        	  println("commit")
        	  Database.lt.C (tid, op._3)
        	  commit()
          }
        } // for
       }
       // Transaction (sMove.s(0), sMove)
    } // run

    /*******************************************************************************
     * Begin this transaction.
     */
    def begin ()
    {
        Thread.sleep (5)
        println ("begin transaction " + tid)
    } // begin

    /*******************************************************************************
     * Read data object oid.
     * @param oid  the database object
     */
    def read (oid: Int): Double =
    {
        Thread.sleep (10)
        val value = Database.data(oid).last
        //log.save(List(Tuple5('r', tid, oid, value, value)))
        println ("read " + tid + " ( " + oid + " ) value = " + value)
        value
    } // read

    /*******************************************************************************
     * Write data object oid.
     * @param oid  the database object
     */
    def write (oid: Int, value: Double)
    {
        Thread.sleep (15)
        println ("write " + tid + " ( " + oid + " ) value = " + value)
        //var temp = Database.data(oid)
       // log.save(List(Tuple5('r', tid, oid, temp, value)))
        Database.data(oid) ++= List(value)
        println("Has written oid: " + Database.data(oid))
    } // write

    /*******************************************************************************
     * Commit this transaction.
     */
    def commit ()
    {
        Thread.sleep (20)
        println ("commit transaction " + tid)
        for(i <- 0 until Database.data.length){
          if(Database.data(i).length > 1){ // there is two version of values
            //Database.data(i)(0) = Database.data(i)(1)
            Database.data(i) = Database.data(i) diff List(0)
          }
        }
    } // commit
    
    def rollback (tidr: Int) = {
      // rollback one transaction
      //remove rollbacked lock from locktable
     println("Rollback " + tidr + " transaction")
     for((oidr,lockListr) <- Database.lt.locks){
        for(lockr <- lockListr)
          if(lockr.tid == tidr)
        	Database.lt.U(lockr.tid, oidr)
      }//for in
     
    for(log<-log.log)
    {
      if(log._2 == tid){
//        Database.data(log._3) = log._4
      }// if log._2 == tid
    } //for log
    
    }//for out
     //remove op from schedule
 

} // Transaction class


} // TransactionTest object

