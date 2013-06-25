package edu.harvard.iq.textcleanup.heuristics

import edu.harvard.iq.textcleanup.WordCorrector

/**
 * Deals with the special case of apostrophe in English:
 * "token's" => "[token]'s"
 */
class ApostropheHeuristic( val corrector:WordCorrector ) extends Heuristic {
	override def suggest( token:String ) =  {
	    if ( token.endsWith("'s") )
	        corrector.correct( token.dropRight(2) ).map( _ + "'s").toList
	        
	    else if ( token.endsWith("s'") )
	        corrector.correct( token.dropRight(1) ).map( _ + "'").toList
	        
	    else 
	        Nil
	}
}