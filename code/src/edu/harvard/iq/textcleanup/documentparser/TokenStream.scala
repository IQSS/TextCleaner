package edu.harvard.iq.textcleanup.documentparser

import org.apache.commons.lang3.StringEscapeUtils.unescapeHtml4
import edu.harvard.iq.textcleanup.RegexUtils._
import scala.Array.canBuildFrom


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
	var buffer = new collection.mutable.Queue[ DocumentToken ]
	var line = 0
	var totalLength = 0L
	
	/** Matches strings that consist
	 *   only of numbers and characters, and contain both 
	 */ // TODO move to the cleaner
    val alphaNumericRgx = """(\p{Alpha}+\d+)+\p{Alpha}*|(\d+\p{Alpha}+)+\d*""".r
    
	private val EOL_MARKER = ""
	
    def next():DocumentToken = {
	    if ( buffer.isEmpty ) {
		     if ( in.hasNext ) {
		        fillBuffer()
		    } else {
		    	return EndOfFileDT( OriginalPosition(line,0,0) )
		    }
	    } 
	    buffer.dequeue()
    }
	
	def peek = {
	    if ( ! buffer.isEmpty ) {
	        buffer(0)
	    } else {
		     if ( in.hasNext ) {
		        fillBuffer()
		        buffer(0)
		    } else {
		    	EndOfFileDT( OriginalPosition(line,0,0) )
		    }
	    } 
	}
	
	private def fillBuffer() {
	    // read next line
	    buffer ++= readNextLine()
	    
	    // check for explicit hyphenation
	    while ( isBufferExplicitlyHyphenated && in.hasNext ) {
	        var nextLineTkns = readNextLine()
	        if ( nextLineTkns(0).isInstanceOf[StringDT] ) {
	            val lastStringDt = lastStringDT match { case Some(s) => s; case None => null } 
	            
	            // remove tokens up to and including lastStringDT
	            buffer = buffer.reverse.dropWhile( _ != lastStringDT ).reverse
	            // merge last string dt and the first of next line tokens
	            val dehyphed = lastStringDt.mergeForward( nextLineTkns(0).asInstanceOf[StringDT] )
	            // put all in queue
	        	
	        	buffer += dehyphed
	        	buffer ++= nextLineTkns.drop(1)
	        	
	        }
	    }
	}
    
	/**
	 * Returns a list of document tokens. All but the last are
	 * really StringDT. The last is a LineBreakDT.
	 * Empty tokens are filtered out, but their position an length are taken into account. 
	 */
	private def readNextLine() = {
	    line += 1
	    var pos = 1;
	    val parsedLine = new collection.mutable.MutableList[DocumentToken];
	    val rawLine = in.next()
	    totalLength += rawLine.length
	    
	    for ( tkn <- rawLine.split("\\s") ) yield {
	        if ( tkn.length > 0 ) {
	        	parsedLine += StringDT( OriginalPosition(line, pos, tkn.length ),
	        	        				if ( tkn.contains("&") ){ unescapeHtml4(tkn) } else { tkn } )
	        }
	        pos += (tkn.length + 1)
	    }
	    parsedLine += LineBreakDT( OriginalPosition(line, pos, 1) )
	    
	    parsedLine
	}
	
	private def lastStringDT: Option[StringDT] = {
	    for ( dt <- buffer.reverseIterator ) {
	        if ( dt.isInstanceOf[StringDT] ) return Some(dt.asInstanceOf[StringDT] )
	    }
	    None;
	}
	
	/**
	 * returns {@code true} iff the last StringDT in the buffer
	 * ends with "-"
	 */
	private def isBufferExplicitlyHyphenated = {
	    lastStringDT match {
	        case None => false
	        case Some(sdt) => sdt.text.endsWith("-")
	    }
	}
	
	/**
     * Breaks alphanumeric string to a list of alpha and numeric strings.
     * TODO this should move to the cleaner
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
	            case StringDT( p, s ) => print( "[%s]".format(s) )
	            case LineBreakDT(p) => println()
	            case EndOfFileDT(p) => go=false; println("/end")
	        }
	    }
    }   
}

