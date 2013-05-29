package edu.harvard.iq.textcleanup
import java.nio.file.Path
import org.apache.commons.lang3.StringEscapeUtils
import io.Source
import java.nio.file.Files
import java.nio.charset.StandardCharsets.UTF_8
import java.io._

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
    
    def cleanStream( input:Iterator[String], wordFunc:String=>Unit, eolFunc: ()=>Unit ) {
    	for ( line <- input ) {
    	    for ( (orig,corr) <- cleanLine(line.split("\\s+").toList) ) 
    	        wordFunc( orig )
    		
    		eolFunc()
    	}
    }
    
    /**
     * Returns 2-tuples: (initial, corrected)
     */
    def cleanLine( elements:List[String] ) = {
        for ( word <- elements ) yield {
    			var cleanWord  = StringEscapeUtils.unescapeHtml4(word).toLowerCase
				analyzeWord( cleanWord ) match {
				    case WordSuspect(w)     => ( w, corrector.correct(w) )
				    case NumberSuspect(n)   => ( n,n )
				    case CompoundSuspect(c) => ( c, cleanCompound(c) )
//				    case GibbrishSuspect(g) => cleaned up from the text.
				}
    		}
    }
    
    val delimiters = "-.,?!:;\"'"
        
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
    
    def go {
        // open output file
        val outWriter = Files.newBufferedWriter( out, UTF_8 )
        val src = Source.fromFile(in.toFile)
        
        cleanStream( src.getLines, 
                	(w)=>{ outWriter.write(w); outWriter.write(" ") },
                	() =>{ outWriter.write("\n") }
                )
        
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
