package edu.harvard.iq.textcleanup

import java.nio.file.Path
import java.nio.file.Files
import java.nio.charset.StandardCharsets.UTF_8
import java.io._
import io.Source
import org.apache.commons.lang3.StringEscapeUtils.unescapeHtml4
import org.apache.commons.lang3.StringUtils
import org.apache.lucene.search.spell.LevensteinDistance
import edu.harvard.iq.textcleanup.RegexUtils._
import edu.harvard.iq.textcleanup.documentparser._
import edu.harvard.iq.textcleanup.heuristics.SpellCheckersHeuristic
import edu.harvard.iq.textcleanup.heuristics.FixSuggestion
import org.apache.commons.lang3.StringEscapeUtils.unescapeHtml4

abstract class StringToken
case class WordSuspect( w:String ) extends StringToken
case class NumberSuspect( n:String ) extends StringToken
case class CompoundSuspect( c:String ) extends StringToken 
case class GibbrishSuspect( g:String ) extends StringToken


/** 
 *  Classification of a {@link StringDT}, regarding its
 *  fixability, if at all needed.
 */
sealed abstract class ClassifiedToken {
    val stringDT:StringDT
}
case class Pass(      stringDT:StringDT ) extends ClassifiedToken
case class Fixable(   stringDT:StringDT, fix:FixSuggestion, alternatives:Seq[FixSuggestion] ) extends ClassifiedToken
case class Unfixable( stringDT:StringDT ) extends ClassifiedToken

/**
 * Takes a document, created a clean version of it.
 * 
 * TODO pass single punctuation marks as well
 */
class DocumentCleaner( val vocabulary:Set[String], val corrector:WordCorrector ) {
	/** clean word regex */
    val lettersOnlyRgx = "^[a-zA-Z]+$".r
    /**
     *  strings that match this, are considered to be words, and so passed to 
     *  the spell checker
     */ 
    val atLeastOneLetter = ".*[a-zA-Z].*".r
    /**
     * Strings that match this, but not the former are considered to be numbers 
     */
    val digitsOnlyRgx = "^[0-9].*$".r
    
    val punctuationRgx = """\p{Punct}""".r
   
    val delimiters = "-.,?!:;\"'()"
     
    val sentenceTerminators = Set('.','!','?')
    val paragraphBreakWidthThreshold = 0.6
    val levenstein = new LevensteinDistance
    
    private var outWriter:BufferedWriter = null
    private var stats:DocumentStatistics = null
    private var lineHasPreviousWord = false

    def go( in:Path, out:Path ) = {
    	val src = Source.fromFile(in.toFile)
    	// Read the entire file to get some stats.
    	val lines = src.getLines().map( unescapeHtml4(_) ).toList
    	src.close()
    	
    	stats = new DocumentStatistics(in, out)
    	stats.maxOriginalLineLength = lines.map( _.length ).max
    	val paragraphBreakThreshold = stats.maxOriginalLineLength*paragraphBreakWidthThreshold
    	
    	val tokenStream = new DocumentTokenStream( lines.iterator )
    	
    	outWriter = Files.newBufferedWriter( out, UTF_8 )

    	// Last string document token we've seen. Candidate for merging in 
    	// when an end-of-line status is met.
        var prevStringDT:StringDT = null
        var go = true
        
        // pre-fill lastWord.
        while ( prevStringDT == null && go) {
            tokenStream.next() match {
                case StringDT(p, s) => prevStringDT = StringDT(p,s)
                case LineBreakDT(p) => ()
                case EndOfFileDT(p) => go = false
            }
        }
        
    	def endParagraph = { emitToken(prevStringDT); 
    						 writeEOL; 
    						 prevStringDT=null }
    	
        // iterate over the file, one token at a time
        while ( go ) {
            tokenStream.next() match {
                case sdt@StringDT( p, s ) => { 
                    // Simple case: Another word. Clean and write previous word.
                    if ( prevStringDT != null ) {
                        emitToken( prevStringDT )
                    }
                    prevStringDT = sdt
                }
                
	            case LineBreakDT(p) => {
	                // A paragraph break, a line break, or part of a sequence of empty lines.
	            	if ( prevStringDT!=null && !prevStringDT.text.isEmpty() ) {
	            		// end of paragraph?
		                if ( sentenceTerminators.contains(prevStringDT.text.last) ||
		                        (p.start+p.length) < paragraphBreakThreshold ) {
		                    endParagraph
		                    
		                } else {
		                	// Same paragraph. Need to decide whether a word was broken 
		                    // between the two lines.
		                    val nextToken = tokenStream.peek;
		                    nextToken match {
		                    	case LineBreakDT(p) => endParagraph
		                    	case EndOfFileDT(p) => endParagraph; go = false
		                        case nt@StringDT( p, ns ) => {
		                            // Was there a line break in the middle of a word?
		                            if ( likelySplit(prevStringDT.text, nt.text) ) {
		                                // word was hyphenated. output the merged version
		                                prevStringDT = prevStringDT.mergeForward(nt)
		                                tokenStream.next() // remove token from stream
		                                
		                            } else {
		                                // seems like words are indeed separate
		                                emitToken( prevStringDT )
		                                prevStringDT = null
		                            }
		                        }
		                    }
		                }
	            	} else {
	            	    writeEOL
	            	}
	            }
	            
	            case EndOfFileDT(p) => go=false
            }
        }
        
        outWriter.close
        stats.originalLength = tokenStream.totalLength
        stats
    }
    
    /**
     * Decides whether it is more likely that the original text (before hyphenation and OCR) contained
     * the merged text, or the two un-merged ones.
     * @returns {@code true} if it is likely that the original text contained the merged word (so, need to use
     *          the merged parameter).
     */
    def likelySplit( current:String, next:String ) = {
        // simple cases we can safely reject
        import java.lang.Character._
        if ( isLetter(current.last) != isLetter(next(0)) ) 
            false
        else {
            val merged = classifyToken(StringDT(null, current+next))
            val original  = ( classifyToken(StringDT(null,current)), classifyToken(StringDT(null,next)) )
            ( merged, original ) match {
	        	case ( Pass(_),        ( _, _) ) => true
	        	case ( Unfixable(_),   ( _, _) ) => false
	        	case ( Fixable(_,f,_), (t1,t2) ) => f.editDistance < ctEditDistance(t1) + ctEditDistance( t2 )
	        }
        }
    }
    
    private def ctEditDistance( c: ClassifiedToken ) = c match {
        case Pass(_) => 0
        case Unfixable(_) => Int.MaxValue
        case Fixable(_,f,_) => f.editDistance
    }
        
    def emitToken( sdt:StringDT ) {
        val classified = classifyToken( sdt )
        stats.update( classified )
        writeWord( classified match {
            case Fixable( s, t, _ ) => t.suggestion
            case _ =>  classified.stringDT.text
        } )
    }
     
    /**
     * Calculate the fixability of a single StringDT. 
     */
    def classifyToken( sdt:StringDT ) = {
    	if ( sdt.textCore.isEmpty || (vocabulary contains sdt.text.toLowerCase) )
    	    Pass( sdt )
    	else corrector.correct( sdt.text ) match {
    	    case Nil     => Unfixable( sdt )
    	    case best :: alts => {
    	        if ( best.editDistance  == 0 ) Pass(sdt.copy( text=best.suggestion) )
    	        else Fixable( sdt, best, alts )
    	    }
    	}
    }
    
    def writeWord( w:String ) {
        if ( w != "" ) {
            if ( lineHasPreviousWord ) outWriter.write(" ")
            outWriter.write(w)
            lineHasPreviousWord = true
        }
    }
   
    def writeEOL {
    	outWriter.write("\n")
    	lineHasPreviousWord = false
    }
    
}

