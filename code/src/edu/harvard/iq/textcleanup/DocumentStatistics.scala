package edu.harvard.iq.textcleanup

import java.nio.file.Path
import edu.harvard.iq.textcleanup.heuristics.FixSuggestion
import edu.harvard.iq.textcleanup.heuristics.FixSuggestion

case class FixSuggestionSelection( val chosen:FixSuggestion, val alternatives:Seq[FixSuggestion] )

/**
 * Stores data about the fixing process of a document.
 */
class DocumentStatistics( val original:Path, val fixed:Path ) {
	
    private val _fixes = collection.mutable.ListBuffer[FixSuggestionSelection]()
    
    var originalLength = 0L
    var maxOriginalLineLength = 0
    var originalWordCount = 0L
    
    var unfixableTokens = 0L
    var passedTokens = 0L
    var fixedTokens = 0L
    
    def update( ct:ClassifiedToken ) = {
        ct match {
            case Pass(t)      => passedTokens += 1
            case Unfixable(t) => unfixableTokens += 1
            case Fixable(t,f,a) => {
                if ( f.editDistance == 0 ) passedTokens += 1
                else fixedTokens += 1; add( f,a )
            }
        }
        this
    }
    
    def add( fix:FixSuggestion, alts:Seq[FixSuggestion] ) = {
        _fixes += FixSuggestionSelection(fix, alts)
    }
    
    def allFixes = _fixes.iterator
    
    /**
     * Returns 1 - (total fixes edit distance) / ( total length ).
     */
    def score = {
        val fixes = _fixes.map( _.chosen.editDistance ).sum
        1.0 - fixes.asInstanceOf[Double]/originalLength
    }
    
    override def toString = {
        "[Stats in:%s pass:%,d fixed:%,d unfixable:%,d score:%g]".
        		format( original.getFileName(), passedTokens, fixedTokens, unfixableTokens, score)
    }
}