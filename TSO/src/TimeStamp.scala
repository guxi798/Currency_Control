class TimeStamp() {
  var RTS,WTS:Int = 0
  var ts:Int = 0
  
  def setts():Int = {
    synchronized{
      ts+=1
    }
    ts
  }
  
  def getRTS():Int ={
    synchronized{
    RTS
    }
  }//geTRTS

   def getWTS():Int ={
    synchronized{
    WTS
    }
  }//geTWTS
   
    def setRTS(RTS:Int){
    synchronized{
    this.RTS = RTS
    }
    println("setRST "+RTS)
  }//seTRTS
    
    def setWTS(WTS:Int){
    synchronized{
    this.WTS = WTS
    }
    println("setWTS " + WTS)
  }//seTRTS
}