import scala.util.control.Breaks._

trait ReadWrite
{
    val r = 'r'
    val w = 'w'
} // ReadWrite
object Database 
{
    /** The lock table to control concurrent access to the database
     */
    val lt   = new LockTable ()

    /** The abstract database representation (an array of double objects)
     */
    val data = new Array [Double] (100)

} // Database object



/***********************************************************************************
 * Test the Transcation class by running several concurrent transactions/threads.
 */
object TransactionTest extends Application with ReadWrite
{
    println ("Test Transactions")
    //val s1 = new Schedule (List ( (r, 1, 0), (w, 1, 1) ))
    //val s2 = new Schedule (List ( (w, 2, 0), (w, 2, 1) ))
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
        
       // var op = Tuple3 [Char, Int, Int]()
        
        begin ()
       breakable{
        for (i <- 0 until s.len) {
         var sx = 1 // indicator for shared(1) or exclusive(0) lock
         var op = s.l(i)
          if(d.update(List(op))){
            println("Not in CSR")
            rollback(op._2)
            tag = 0
            break
          }
            if (op._1 == 'r') {
              if(i < s.len){
                for(j <- i+1 until s.len){
                  if(s.l(j)._1 == 'w'){
                    sx = 0
                    break
                  } // if w
                } // for j
              } // if i < s.len
              
              if(sx == 1){
                Database.lt.S (tid, op._3)
              }
              else{
                Database.lt.X (tid, op._3)
              }
                
              value = read (op._3)   
            } else {
              println("write")
              d.print()
                Database.lt.X (tid, op._3)
                write (op._3, value + 1.)
            } // if
        } // for
       }
        if(tag == 1){
        	commit ()
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
        val value = Database.data(oid) 
        log.save(List(Tuple5('r', tid, oid, value, value)))
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
        var temp = Database.data(oid)
        log.save(List(Tuple5('r', tid, oid, temp, value)))
        Database.data(oid) = value
    } // write

    /*******************************************************************************
     * Commit this transaction.
     */
    def commit ()
    {
        Thread.sleep (20)
        println ("commit transaction " + tid)
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
        Database.data(log._3) = log._4
      }// if log._2 == tid
    } //for log
    
    }//for out
     //remove op from schedule
 

} // Transaction class


} // TransactionTest object

