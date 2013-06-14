package edu.harvard.iq.textcleanup

import edu.harvard.iq.textcleanup.heuristics.Heuristic
import edu.harvard.iq.textcleanup.heuristics.FixSuggestion
import edu.harvard.iq.textcleanup.heuristics.FixSuggestion
import edu.harvard.iq.textcleanup.heuristics.FixSuggestion
import com.sun.xml.internal.bind.v2.util.EditDistance

/**
 * Corrects words. Maintains a set of {@link Heuristic}s, and consults them.
 * Then, selects the most probable fix. This class also takes care of case 
 * folding and unfolding.
 * 
 */
class WordCorrector() {
	
    /** The set of heuristics we consult. */
    private[this] var heuristics = Set[Heuristic]()
    
    def correct( word:String ) : Option[FixSuggestion] = {
        // fold case
        val wordCase   = WordCase( word )
        val foldedWord = word.toLowerCase()
        
        // consult heuristics, choose best
        val sgsts = heuristics.map(
                h => h.suggest(foldedWord) match {
				            case None => null
				            case Some(fix) => new FixSuggestion( foldedWord, fix, h )
				        }).filter( _ != null )
        
		if ( sgsts.isEmpty ) {
            None
            
        } else {
            val sgst = sgsts.maxBy( -_.editDistance )
    		Some(new FixSuggestion(sgst.original,
    				wordCase(sgst.suggestion),
    				sgst.heuristic
    			))
        }
    }
    
    def add( h:Heuristic ) = {
        heuristics += h
        this
    } 
    
    override def toString = "[WordCorrcor heuristics:%s]".format(heuristics.toString)
   
}

abstract class WordCase {
    def apply( lowercase:String ):String
}
case class LowerCase extends WordCase {
    def apply( lowercase:String ) = lowercase
}

case class UpperCase extends WordCase {
    def apply( lowercase:String ) = lowercase.toUpperCase()
}

case class TitleCase extends WordCase {
    def apply( lowercase:String ) = lowercase(0).toUpper + lowercase.drop(1)
}

object WordCase {
    import java.lang.Character._
    
    def apply( in:String ) = {
        val WordCase( t ) = in
        t
    }
    
    def unapply( in:String ): Option[WordCase] = {
        val tin = in.trim
        if ( tin.forall( ! isUpperCase(_) ) ) 
            Some(LowerCase())
        else {
            if ( isUpperCase(tin(0)) && tin.exists( isLowerCase(_) ) )
                Some( TitleCase() )
            else
            	Some( if ( tin.count(isUpperCase(_)) > tin.count( isLowerCase(_) ) )
            	        UpperCase() else LowerCase() )
            	    
        }
    }
}
