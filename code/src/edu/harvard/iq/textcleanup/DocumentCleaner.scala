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
abstract class StringToken
case class WordSuspect( w:String ) extends StringToken
case class NumberSuspect( n:String ) extends StringToken
case class CompoundSuspect( c:String ) extends StringToken 
case class GibbrishSuspect( g:String ) extends StringToken


/**
 * Takes a document, created a clean version of it.
 * 
 * TODO pass single punctuation marks as well
 */
class DocumentCleaner( val corrector:WordCorrector ) {
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
    
    val levenstein = new LevensteinDistance
    
    var outWriter:BufferedWriter
    var stats:DocumentStatistics

    def go( in:Path, out:Path ) = {
    	val src = Source.fromFile(in.toFile)
    	val lines = src.getLines().toList
    	src.close()
    	
    	stats = new DocumentStatistics(in, out)
    	stats.maxOriginalLineLength = lines.map( _.length ).max
    	
    	val tokenStream = new DocumentTokenStream( lines.iterator )
    	
    	outWriter = Files.newBufferedWriter( out, UTF_8 )
        var lastWord:StringDT = null
        var go = true
        
        // pre-fill lastWord.
        while ( lastWord == null && go) {
            tokenStream.next() match {
                case StringDT(p, s) => lastWord = StringDT(p,s)
                case LineBreakDT(p) => ()
                case EndOfFileDT(p) => go = false
            }
        }
        
        // iterate over the file, one token at a time
        while ( go ) {
            tokenStream.next() match {
                case StringDT( p, s ) => { 
                    if ( lastWord != null ) {
                        
                        emitWord( cleanSingleElement(lastWord) )
                    }
                    lastWord = StringDT(p,s)
                }
                
	            case LineBreakDT(p) => {
	            	if ( lastWord != null && ! lastWord.str.isEmpty() ) {
		                if ( sentenceTerminators.contains(lastWord.str.last) ) {
		                    // end of paragraph
		                    emitEOLWord( cleanSingleElement(lastWord.str) )
		                    lastWord = null
		                    
		                } else {
		                    if ( punctuationRgx.matches( lastWord.str.last.toString ) ) {
		                    	// emit corrected last word
		                        emitWord( cleanSingleElement(lastWord) )
		                        lastWord = null

		                    } else {
			                	// we need to decide whether a word was broken 
			                    // between the two lines.
			                    val nextToken = tokenStream.next();
			                    nextToken match {
			                        case StringDT( p, ns ) => { 
			                            val (e, r) = electMergeOrSplit(lastWord, ns)
			                            emitWord(e)
			                            lastWord = r match {
						                                case None => null
						                                case Some(s) => s
						                            }
			                        }
			                        case LineBreakDT(p) => emitEOLWord( cleanSingleElement(lastWord) ); lastWord = null    // same as paragraph break
			                        case EndOfFileDT(p) => emitEOLWord( cleanSingleElement(lastWord) ); go = false
			                    }
		                    }
		                }
	            	} else {
	            	    emitEOL
	            	}
	            }
	            
	            case EndOfFileDT(p) => go=false
            }
        }
        
        outWriter.close
        
        stats
    }
    
    
    def �( word:String ) =  {
        val corrected = cleanSingleElement(word)
    	( word, 
    	  1-levenstein.getDistance(word, corrected), // lucene's LD returns between 0: max difference and 1:identical
    	  corrected )
    }
    
    def bestFixFor( word:String ) : FixSuggestion = {
        corrector.correct(word)
    }
    
    /**
     * Gets two words. Decides whether it is more likely that
     * they were separated or really are two words.
     * The decision is made based on edit distance between
     * the corrected version and the originals.
     * A short-circuit don't merge logic is applied when
     * at least one of the words is pure numbers and the other is not.
     * 
     * @return a 2-tuple: ( word-to-emit, word to retain )
     */
    def electMergeOrSplit( w1:String, w2:String ): (String, Option[String]) = {
    	
        // short circuit for digit-letter cases
        val w1IsNum = digitsOnlyRgx.matches( w1 ) 
    	val w2IsNum = digitsOnlyRgx.matches( w2 )
    	if ( w1IsNum || w2IsNum ) return ( w1, Some(w2) )
    	
		val separated = ( �(w1), �(w2) )
		val joined = �( w1+w2 )
		val sepSum = (separated._1._2 + separated._2._2)/2.0
		
		if ( joined._2 <= sepSum )
		    ( joined._3, None )
		else
		    ( separated._1._3, Some(separated._2._1) )
    }
    
    def emitEOL = emitEOLWord( "" )
    def emitEOLWord( w:String ) = emitWord(w, delimiter="\n" )
    
    def emitWord( w:String, delimiter:String=" " ) {
        if ( w != "" ) {
            outWriter.write(w)
            outWriter.write(delimiter)
        }
    }
    
    /**
     * Gets a raw token, 
     */
    def classifyToken( sdt:StringDT ) = {
    	if ( false /*dict contains w or w is a single punct, or a number*/ )
    	    Pass( sdt )
    	else corrector.correct( sdt.str ) match {
    	    case None     => Unfixable( sdt )
    	    case Some(fs) => Fixable( sdt, fs ) 
    	}
    }
    
    def cleanSingleElement( word:String ):String  = {
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
    
    /**
     * Take the compound element and correct only its words
     * LATER also consider correcting the entire word, and 
     *    >> select the most probable solution, based on 
     *    >> Levenstein Distance (as in the line breaks)
     */
    def cleanCompound( cw:String ) = {
        val sb = new StringBuilder;
        val st = new java.util.StringTokenizer( cw, delimiters, true )
        while ( st.hasMoreTokens ) {
            val emt = st.nextToken()
            sb.append(  if ( delimiters contains emt )
                			emt 
                		else analyzeWord( emt ) match {
						    case WordSuspect(w)     => corrector.correct(w)
						    case NumberSuspect(n)   => n
						    case CompoundSuspect(c) => c
						    case GibbrishSuspect(g) => "" // ignore these
						})
        }
        
        sb.toString
    }
    
}

/** 
 *  Classification of a {@link StringDT}, regarding its
 *  fixability, if at all needed.
 */
abstract class ClassifiedToken {
    val token:StringDT
}
case class Pass( token:StringDT ) extends ClassifiedToken
case class Fixable( token:StringDT, fix:FixSuggestion ) extends ClassifiedToken
case class Unfixable( token:StringDT ) extends ClassifiedToken


object TextAnalysis extends App {
    val dc = new DocumentCleaner( null, null, null )
    
    println( dc.analyzeWord("123") )
    println( dc.analyzeWord("hello") )
    println( dc.analyzeWord("hello,") )
    println( dc.analyzeWord("hello-world") )
    println( dc.analyzeWord("...-.---.") )
    println( dc.analyzeWord(".") )
    println( dc.analyzeWord("..-,") )
}
