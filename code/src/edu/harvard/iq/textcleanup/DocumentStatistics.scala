package edu.harvard.iq.textcleanup

import java.nio.file.Path
import edu.harvard.iq.textcleanup.heuristics.FixSuggestion
import edu.harvard.iq.textcleanup.heuristics.FixSuggestion

/**
 * Stores data about the fixing process of a document.
 */
class DocumentStatistics( val original:Path, val fixed:Path ) {
	
    private val _fixes = new collection.mutable.HashSet[FixSuggestion]()
    
    var originalLength = 0L
    var maxOriginalLineLength = 0
    var originalWordCount = 0L
    
    var unfixableTokens = 0L
    var passedTokens = 0L
    var fixedTokens = 0L
    
    def update( ct:ClassifiedToken ) = {
        ct match {
            case Pass(t)      => passedTokens += 1
            case Fixable(t,f) => fixedTokens += 1; add( f )
            case Unfixable(t) => unfixableTokens += 1
        }
        this
    }
    
    def add( fix:FixSuggestion ) = {
        _fixes += fix
    }
    
    def allFixes = _fixes.iterator
    
    /**
     * Returns 1 - (total fixes edit distance) / ( total length ).
     */
    def score = {
        val fixes = _fixes.map( _.editDistance ).sum
        1.0 - fixes.asInstanceOf[Double]/originalLength
    }
}