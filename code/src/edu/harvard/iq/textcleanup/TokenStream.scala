package edu.harvard.iq.textcleanup

import org.apache.commons.lang3.StringEscapeUtils.unescapeHtml4
import edu.harvard.iq.textcleanup.RegexUtils._

abstract class DocumentToken
case class StringDT( str:String ) extends DocumentToken
case class LineBreakDT() extends DocumentToken
case class EndOfFileDT() extends DocumentToken



/**
 * Reads from a file. Does the following simple actions:
 * <ul>
 * 	<li> Un-escapes HTML entities </li>
 *  <li> de-hyphenates explicitly hyphenated lines (those that end with "-") </li>
 *  <li> breaks alpha-numeric sequences to alpha-only and numeric-only tokens</li>
 * </ul>
 * 
 * 
 * End-of-file condition is detected by returning EndOfFileDT() from the next() method.
 */
class DocumentTokenStream( val in:Iterator[String] ) {
	var queue = new collection.mutable.Queue[String]
	
	/** Matches strings that consist
	 *   only of numbers and characters, and contain both 
	 */
    val alphaNumericRgx = """(\p{Alpha}+\d+)+\p{Alpha}*|(\d+\p{Alpha}+)+\d*""".r
    
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
	        LineBreakDT()
	    else
	        StringDT( rawToken )
    
    }
	
	private def fillBuffer() {
	    // read next line
	    addToQueue( readNextLine() )
	    
	    // check for hyphenation
	    while ( queue.last != null && queue.last.endsWith("-") && in.hasNext ) {
	        var nextLineTkns = readNextLine()
	        if ( ! nextLineTkns.isEmpty ) {
	        	val dehyp = queue.last.dropRight(1).trim() + nextLineTkns(0)
	        	queue = queue.dropRight(1)
	        	nextLineTkns = nextLineTkns.drop(1)
	        	addToQueue( dehyp )
	        	addToQueue( nextLineTkns )
	        }
	    }
	    queue += EOL_MARKER
	}
    
	private def addToQueue( l:Iterable[String] ):Unit = for ( w<-l ) addToQueue(w)
	
	private def addToQueue( word:String ):Unit = {
	    if ( alphaNumericRgx matches word ) {
	        queue ++= breakAlphaNum( word )
	    } else {
	        queue += word
	    }
	}
	
	private def readNextLine() = {
	    for ( tkn <- in.next().split("\\s+") ) yield {
	        if ( tkn.contains("&") ) unescapeHtml4(tkn) else tkn
	    }
	}
	
	/**
     * Breaks alphanumeric string to a list of alpha and numeric strings.
     */
    def breakAlphaNum( in:String ):List[String] = {
        val chars = in.toCharArray
        var startIdx = 0
        var curIdx = 1
        val isChar = (x:Char) => (x>='a' && x<='z') || (x>='A' && x<='Z')
        val isNum  = (x:Char) => (x>='0' && x<='9')
        var curFunc = if ( isNum(chars(0)) ) isNum 
        				else if ( isChar(chars(0)) ) isChar
        				else throw new IllegalArgumentException("string [" + in + "] is not alpha-numeric")
        val out = new collection.mutable.MutableList[String]
        while ( curIdx < chars.length ) {
            if ( ! curFunc( chars(curIdx) ) ) {
                out += new String( chars, startIdx, curIdx-startIdx )
                curFunc = if ( curFunc == isNum ) isChar else isNum
                startIdx = curIdx
            } 
            curIdx += 1
        }
        out += new String( chars, startIdx, curIdx-startIdx )
        out.toList
    }
}


object DTS_Test extends App {
    val input = List( ("simple", Array("hello world this is line 1", "this is a line with &#39; escapes", "I'm a hyphen-","ated line, so need to", "see if that works" )),
            		  ("double-hyph", Array("this line conca-","tenates to the next li-","nie, which contac-","ts even more") ),
            		  ("end-hyph", Array("line line line-") ),
            		  ("with empty", Array("line line","","line line line","") ),
            		  ("with empty hyp", Array("line li-","","ne") ),
            		  ("real", Array("HISSED HIS MIKE.",
										". . ..-,",
										"A TrN Story f Kratvkjr fa",
										"Mm Tine.",
										"1U",
										"Bowling Green Correapondonoc of the Courier-Journal.")),
					  ("alphanum", Array("line with 123hjk234", "in1987 nothing really inter-","esting happened") )
            		  
            		)
            		
    for ( (title, arr) <- input ) {
        println( "=== %s ===".format( title ) )
    	val sut = new DocumentTokenStream( arr.iterator )
    
	    var go = true
	    while ( go ) {
	        sut.next() match {
	            case StringDT( s ) => print( "[%s]".format(s) )
	            case LineBreakDT() => println()
	            case EndOfFileDT() => go=false; println("/end")
	        }
	    }
    }
    
    val alnumTests = Array("in1920" ,"asd", "123", "aa12", "12aa", "as12as", "12as12", "12as12as", "sa12sa12", "as12as12as", "12as12a12")
    val sut = new DocumentTokenStream( Array("").iterator )

    for ( s  <- alnumTests ) {
        println( "%s \t-> %s".format(s, sut.alphaNumericRgx.matches(s))) 
    }
    
    
}

