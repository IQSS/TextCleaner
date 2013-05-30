package edu.harvard.iq.textcleanup
import java.nio.file.Path
import org.apache.commons.lang3.StringEscapeUtils.unescapeHtml4
import io.Source
import java.nio.file.Files
import java.nio.charset.StandardCharsets.UTF_8
import java.io._
import org.apache.commons.lang3.StringUtils

abstract class StringToken
case class WordSuspect( w:String ) extends StringToken
case class NumberSuspect( n:String ) extends StringToken
case class CompoundSuspect( c:String ) extends StringToken
case class GibbrishSuspect( g:String ) extends StringToken


/**
 * Takes a document, created a clean version of it.
 */
class DocumentCleaner( val in:Path, val out:Path, val corrector:WordCorrector ) {
	/** clean word regex */
    val lettersOnlyRgx = "^[a-z]+$".r
    /**
     *  strings that match this, are considered to be words, and so passed to 
     *  the spell checker
     */ 
    val atLeastOneLetter = ".*[a-z].*".r
    /**
     * Strings that match this, but not the former are considered to be numbers 
     */
    val digitsOnlyRgx = "^[0-9].*$".r
    
    val delimiters = "-.,?!:;\"'"
        
    private[this] var input:Iterator[String]=null
    
    def cleanStream( wordFunc:String=>Unit, eolFunc: ()=>Unit ) {
        var pCorrected = cleanLine( readNextLine() )
        
        while ( input.hasNext ) {
            var corrected = cleanLine( readNextLine() )
            
            // check if merging the last word of pLine with the first word of line makes sense
            val pLast = pCorrected.last
            val first = corrected.head
            val merged = pLast._1.trim + first._1.trim
            val mergedPair = ( merged, cleanSingleElement(merged) )
            // for each pair, decide on the difference
            // go for the option with minimal LD between the original and the corrected
            if ( mergedPair._1 == mergedPair._2 ) {
                pCorrected = pCorrected.dropRight(1)
                pCorrected = pCorrected ++ List(mergedPair)
                corrected = corrected.drop(1)
            }
            
            // dump pCorrected, maybe with the merged
            
            // advance
        }
    	for ( line <- input ) {
    	    val canonicalRep = unescapeHtml4(line).toLowerCase
    	    for ( (orig,corr) <- cleanLine(canonicalRep) ) 
    	        wordFunc( corr.asInstanceOf[String] )
    		
    		eolFunc()
    	}
    }
    
    def propagateLine( line:List[(String,String)], wordFunc:String=>Unit, eolFunc: ()=>Unit ) {
        for ( (orig,corr) <- line ) wordFunc( corr.asInstanceOf[String] )
    	eolFunc()
    }
    
    /**
     * Reads the next line and normalizes it. Takes into account
     * explicitly hypenated lines (that ends with "-").
     */
    def readNextLine() = {
        var canonicalRep:String = null
        
        while ( input.hasNext ) {
            val line = input.next()
            canonicalRep = unescapeHtml4(line).toLowerCase.trim
            // explicitly hyphenated lines (ends with "-")_are eagerly merged here
            while ( canonicalRep.endsWith("-") && input.hasNext ) {
                canonicalRep = canonicalRep.dropRight(1) +
                				unescapeHtml4(input.next()).toLowerCase.trim
            }   
        }
        canonicalRep
    }
    
    /**
     * Returns 2-tuples: (initial, corrected)
     */
    def cleanLine( elements:String ):List[(String,String)] = {
        (for ( word <- elements.split("\\s+") ) yield (word, cleanSingleElement(word) )).toList
    }
        
    /**
     * Take the compound element and correct only its words
     */
    def cleanCompound( cw:String ) = {
        val sb = new StringBuilder;
        val st = new java.util.StringTokenizer( cw, delimiters, true )
        while ( st.hasMoreTokens ) {
            val emt = st.nextToken()
            sb.append( if ( delimiters contains emt ) emt else corrector.correct(emt) )
        }
        
        sb.toString
    }
    
    def cleanSingleElement( word:String)  = {
        analyzeWord( word ) match {
		    case WordSuspect(w)     => corrector.correct(w)
		    case NumberSuspect(n)   => n
		    case CompoundSuspect(c) => cleanCompound(c)
		    case GibbrishSuspect(g) => "" // ignore these
		}
    }
    
    def analyzeWord( s:String ) = {
    	lettersOnlyRgx.findFirstIn(s) match {
    		case Some(_) => WordSuspect( s )
    		case None    => digitsOnlyRgx.findFirstIn(s) match {
    		    case Some(_) => NumberSuspect(s)
    		    case None    => atLeastOneLetter.findFirstIn(s) match {
    		        case Some(_) => CompoundSuspect( s )
    		        case None    => GibbrishSuspect( s )
    		    }
    		}
    	}
    }
    
    def go {
        // open output file
        val outWriter = Files.newBufferedWriter( out, UTF_8 )
        val src = Source.fromFile(in.toFile)
        input = src.getLines
        cleanStream( (w)=>{ outWriter.write(w); outWriter.write(" ") },
                	 () =>{ outWriter.write("\n") } )
        
        src.close
        outWriter.close
    }
}

object TextAnalysis extends App {
    val dc = new DocumentCleaner( null, null, null )
    
    println( dc.analyzeWord("123") )
    println( dc.analyzeWord("hello") )
    println( dc.analyzeWord("hello,") )
    println( dc.analyzeWord("hello-world") )
    println( dc.analyzeWord("...-.---.") )
}
