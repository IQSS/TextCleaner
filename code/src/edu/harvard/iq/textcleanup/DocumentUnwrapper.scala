package edu.harvard.iq.textcleanup

import org.apache.commons.lang3.StringEscapeUtils.unescapeHtml4
import edu.harvard.iq.textcleanup.documentparser.RawDocument
import edu.harvard.iq.textcleanup.documentparser.UnWrappedDocument
import scala.actors.Channel
import java.util.concurrent.BlockingQueue
import java.nio.file.Files
import java.nio.file.Path
import edu.harvard.iq.textcleanup.documentparser.UnWrappedDocument
import java.nio.charset.StandardCharsets
import edu.harvard.iq.textcleanup.writers.TDMWriter
import java.nio.file.Paths
import edu.harvard.iq.textcleanup.documentparser.RawDocument

/**
 * Unwraps the lines of a document.
 */
class DocumentUnWrapper( val words:Set[String], val outputRoot:Path ) {
    
	val paragraphBreakWidthThreshold = 0.5
	val sentenceTerminators = Set('.','!','?')
	private var thread:java.lang.Thread=null
	
	def go( input:BlockingQueue[RawDocument], statsOut:TDMWriter ) {
	    import edu.harvard.iq.textcleanup.Implicits.makeRunnable
	    thread = new java.lang.Thread(()=>{
	        var run = true
	        while ( run ) {
	            val rd = input.take()
	            if ( rd == DocumentUnWrapper.END_OF_INPUT ) {
	                run = false
	            } else {
	                var uwd = unwrap(rd)
	                statsOut ! uwd
	                writeFile( uwd )
	            }
	        }
	    })
	    thread.start()
	}
	
	def waitForTermination {
	    if ( thread != null ) thread.join()
	}
	
    def unwrap( rd:RawDocument ):UnWrappedDocument = {
        val unescapedLines = rd.content.split("\n").map(unescapeHtml4(_))
        val maxOriginalLineLength = unescapedLines.map( _.length ).max
    	val paragraphBreakThreshold = maxOriginalLineLength*paragraphBreakWidthThreshold
    	
    	val unWraped = collection.mutable.ListBuffer[String]()
    	val curLine = new StringBuilder
        
    	for ( line <- unescapedLines ) {
    		var tLine = line.trim
    	    if ( sentenceTerminators(tLine.last) || tLine.length<paragraphBreakThreshold ) {
    	        // a new paragraph indeed
    	        unWraped += curLine.toString
    	        curLine.clear()
    	        
    	    } else {
    	        // quick check: should we merge the words?
    	        if ( ! detectHyphenation( curLine, tLine) ) {
    	            curLine.append(" ")
    	        }
    	    	curLine.append(tLine)
    	    }
    	}
        unWraped += curLine.toString
        
        UnWrappedDocument( rd.path, unWraped.mkString("\n") )
    }
    
    /**
     * returns true when we think there was hyphenation
     */
    def detectHyphenation( w1:StringBuilder, w2:String ):Boolean = {
    	if ( w1.isEmpty ) return false
    	if ( w2.isEmpty ) return false
        var secondWord = w2.trim.split(" ")(0)
        var lastChar = w1.last
        if ( Character.isAlphabetic(lastChar) && Character.isAlphabetic(secondWord(0)) ) {
            var lastWord = w1.toString.trim.split(" ").last
            
            if ( allCaps(lastWord) ^ allCaps(secondWord) ) {
            	false
            } else {
            	words.contains( (lastWord+secondWord).toLowerCase() )
            }
        } else {
            false
        }
    } 
    
    def allCaps( s:String ) = s.forall( c => 
        		{ (Character.isAlphabetic(c)&&Character.isUpperCase(c)) ||
        		    (!Character.isAlphabetic(c)) 
        		})
    
    def writeFile( doc:UnWrappedDocument ) {
        val outPath = outputRoot.resolve(doc.originalPath)
        if ( ! Files.exists( outPath.getParent()) ) {
            Files.createDirectories(outPath.getParent())
        }
        Files.write(outPath, java.util.Collections.singletonList(doc.content), StandardCharsets.UTF_8)
    }
}

object DocumentUnWrapper {
    /** Used to mark the end-of-input for the worker threads. */
    val END_OF_INPUT = new RawDocument(null, null);
    
    def main( args:Array[String] ) {
        val input = Paths.get("/Volumes/tc-data/data-small/2/part-000734file000062.txt")
        val rd = new RawDocument( input, new String(Files.readAllBytes(input), StandardCharsets.UTF_8) )
        val dictionaryPath = "/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/datasets/nyt/dict/"
        val words = new LineWordSetLoader().load( Paths.get(dictionaryPath) )
        val sut = new DocumentUnWrapper( words, input )
        val out = sut.unwrap(rd)
        println( out.content )
    }
}