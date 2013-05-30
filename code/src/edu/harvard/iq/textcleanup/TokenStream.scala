package edu.harvard.iq.textcleanup

import org.apache.commons.lang3.StringEscapeUtils.unescapeHtml4

abstract class DocumentToken
case class StringDT( str:String ) extends DocumentToken
case class EndOfLineDT() extends DocumentToken
case class EndOfFileDT() extends DocumentToken



/**
 * Reads from a file. Un-escapes HTML entities, and 
 * de-hyphenates explicitly hyphenated lines (those that end with "-").
 */
class DocumentTokenStream( val in:Iterator[String] ) {
	var queue = new collection.mutable.Queue[String]
	
	private val EOL_MARKER = ""
	
    def next():DocumentToken = {
	    if ( queue.isEmpty ) {
		     if ( in.hasNext ) {
		        fillBuffer()
		    } else {
		    	return EndOfFileDT()
		    }
	    } 
	    val rawToken = queue.dequeue()
	    if ( rawToken.eq(EOL_MARKER) )
	        EndOfLineDT()
	    else
	        StringDT( rawToken )
    
    }
	
	private def fillBuffer() {
	    // read next line
	    queue ++= readNextLine()
	    
	    // check for hyphenation
	    while ( queue.last != null && queue.last.endsWith("-") && in.hasNext ) {
	        var nextLineTkns = readNextLine()
	        if ( ! nextLineTkns.isEmpty ) {
	        	val dehyp = queue.last.dropRight(1).trim() + nextLineTkns(0)
	        	queue = queue.dropRight(1)
	        	nextLineTkns = nextLineTkns.drop(1)
	        	queue += dehyp
	        	queue += EOL_MARKER
	        	queue ++= nextLineTkns
	        }
	    }
	    queue += EOL_MARKER
	}
	
	private def readNextLine() = {
	    for ( tkn <- in.next().split("\\s+") ) yield {
	        if ( tkn.contains("&") ) unescapeHtml4(tkn) else tkn
	    }
	}
}


object DTS_Test extends App {
    val input = List( ("simple", Array("hello world this is line 1", "this is a line with &#39; escapes", "I'm a hyphen-","ated line, so need to", "see if that works" )),
            		  ("double-hyph", Array("this line conca-","tenates to the next li-","nie, which contac-","ts even more") ),
            		  ("end-hyph", Array("line line line-") ),
            		  ("with empty", Array("line line","","line line line","") ),
            		  ("with empty hyp", Array("line li-","","ne") )
            		)
            		
    for ( (title, arr) <- input ) {
        println( "=== %s ===".format( title ) )
    	val sut = new DocumentTokenStream( arr.iterator )
    
	    var go = true
	    while ( go ) {
	        sut.next() match {
	            case StringDT( s ) => print( "[%s]".format(s) )
	            case EndOfLineDT() => println()
	            case EndOfFileDT() => go=false; println("/end")
	        }
	    }
    }
    
    
}

