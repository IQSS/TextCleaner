package edu.harvard.iq.textcleanup.heuristics

/** 
 * Attempts to match the input to pairs of words in a dictionary.    
 */
class WordUnmergerHeuristic( val words:Set[String] ) extends Heuristic {
    
    override def suggest( input:String ) = {
    	val start =  if (input.startsWith("i") || input.startsWith("a")) 1 else 2
        val endOffset =  if (input.endsWith("i") || input.endsWith("a")) 1 else 2
        
        val options = (start to input.length-endOffset).
        				map( p => (input.substring(0,p), input.substring(p)) ).
        					filter( t => (words contains t._1) && (words contains t._2) )
        					
        if ( options.isEmpty ) None
        else if ( options.size == 1 ) Some( options(0)._1 + " " + options(0)._2 )
        else {
            println( "Multiple unmerge options: " )
            for ( o <- options ) println( "-> %s %s".format(o._1, o._2) )
            Some( options(0)._1 + " " + options(0)._2 )
        }
    }

}