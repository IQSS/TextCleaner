package edu.harvard.iq.textcleanup.heuristics

/** 
 * Attempts to match the input to pairs of words in a dictionary.    
 */
class WordUnmergerHeuristic( val words:Set[String] ) extends Heuristic {
    
    override def suggest( input:String ) = {
    	val start =  if (input.startsWith("i") || input.startsWith("a")) 1 else 2
        val endOffset =  if (input.endsWith("i") || input.endsWith("a")) 1 else 2
        
        (start to input.length-endOffset).
        	map( p => (input.substring(0,p), input.substring(p)) ).
        		filter( t => (words contains t._1) && (words contains t._2) ).
        			map( t => t._1 + " " + t._2 ).toList
        
    }

}