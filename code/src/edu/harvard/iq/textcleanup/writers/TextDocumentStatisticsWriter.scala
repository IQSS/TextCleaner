package edu.harvard.iq.textcleanup.writers

import java.nio.file.Path
import edu.harvard.iq.textcleanup.DocumentStatistics
import java.nio.charset.StandardCharsets
import java.io.PrintWriter
import edu.harvard.iq.textcleanup.Main

/**
 * Writes the statistics of a document in a plain text file.
 */
class TextDocumentStatisticsWriter( val rootFolder:Path ) {
    
    def write( stats:DocumentStatistics ) = {
        var out = new PrintWriter(java.nio.file.Files.newBufferedWriter(rootFolder.resolve(stats.fixed.getFileName),
                StandardCharsets.UTF_8))
        
        out.println( "# Text Cleanup report" )
        out.println( "Version\t%s".format(Main.VERSION) )
        out.println( "Input\t%s".format( stats.original) )
        out.println( "Output\t%s".format( stats.fixed) )
        
        out.println( "# Counts" )
        out.println( "Pass\t%s".format(stats.passedTokens) )
        out.println( "Fixed\t%s".format(stats.fixedTokens) )
        out.println( "Unfixable\t%s".format(stats.unfixableTokens) )
        out.println( "Original length\t%d".format(stats.originalLength) )
        out.println( "Total Levenshtein dist\t%d".format( stats.allFixes.map(_.editDistance).sum) )
        
        out.println( "# Fixes" )
        out.println( "Original\tFix\tHeuristic")
        stats.allFixes.foreach( f => {
          out.println( "%s\t%s\t%s".format(f.original, f.suggestion, f.heuristic.title) )  
        })
        
        out.close()
    }

}