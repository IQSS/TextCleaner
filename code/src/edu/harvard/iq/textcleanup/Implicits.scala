package edu.harvard.iq.textcleanup

object Implicits {
	
	implicit def makeRunnable( f:()=>Any ) = new Runnable {
	    override def run() = f() 
	}

}