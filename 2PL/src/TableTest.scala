object TableTest extends App {
	val et = new LockTable ()
	
	et.X(0,1); println("et = " + et)
	et.X(1,1); //et.S(2,0)
	//println("et = " + et.toString)
	println("et = " + et)
	//et.X(0,1)
	//println("et = " + et.toString)
}