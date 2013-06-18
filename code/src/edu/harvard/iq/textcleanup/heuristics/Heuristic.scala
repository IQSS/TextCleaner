package edu.harvard.iq.textcleanup.heuristics

class FixSuggestion( val original:String, 
        			 val suggestion:String, 
        			 val heuristic:Heuristic ) {
    private var dist:Option[Int] = None;
    
    def editDistance:Int = {
        dist match {
            case Some(d) => d
            case None    => { dist = Some(StringDistance.levenshtein(original, suggestion))
                			  editDistance } 
        }
    }
}

/**
 * Gets a word, returns a few possible fixes.
 */
trait Heuristic {
	def suggest( raw:String ): Option[String]
}