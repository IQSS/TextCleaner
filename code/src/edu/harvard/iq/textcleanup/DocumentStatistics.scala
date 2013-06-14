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
    var unfixableWordCount = 0L
    
    def addFix( fix:FixSuggestion ) = {
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