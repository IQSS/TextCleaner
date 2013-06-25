package edu.harvard.iq.textcleanup.heuristics

/**
 * Passes only words that are part of its dictionary.
 */
class DictionaryHeuristic( val dictionary:Set[String] ) extends Heuristic {

    override def suggest( t:String ) = {
        if ( dictionary contains t ) t :: Nil
        else Nil
    }
    
}