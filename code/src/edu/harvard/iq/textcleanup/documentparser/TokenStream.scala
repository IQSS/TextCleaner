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
	var lineNumber = 0
	var totalLength = 0L
	
	/** Matches strings that consist
	 *   only of numbers and characters, and contain both
	 *   
	 */ 
    private val explicitHyphenation = """\w-$""".r

	private val EOL_MARKER = ""
	
    def next():DocumentToken = {
	    if ( ! buffer.isEmpty ) {
	        buffer.dequeue()
	    } else {
		     if ( in.hasNext ) {
		        fillBuffer()
		        buffer.dequeue()
		    } else {
		    	EndOfFileDT( OriginalPosition(lineNumber,0,0) )
		    }
	    }
    }
	
	def peek = {
	    if ( ! buffer.isEmpty ) {
	        buffer(0)
	    } else {
		     if ( in.hasNext ) {
		        fillBuffer()
		        buffer(0)
		    } else {
		    	EndOfFileDT( OriginalPosition(lineNumber,0,0) )
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
	            val lastStringDt = lastStringDtOpt match { case Some(s) => s; case None => null } 
	            
	            // remove tokens up to and including lastStringDt
	            buffer = buffer.reverse.dropWhile( _ != lastStringDt ).drop(1).reverse
	            // merge last string dt and the first of next line tokens
	            val fixedLastStringDt = lastStringDt.copy(text=lastStringDt.text.dropRight(1))
	            
	            val dehyphed = fixedLastStringDt.mergeForward( nextLineTkns(0).asInstanceOf[StringDT] )
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
	    lineNumber += 1
	    var pos = 1;
	    val parsedLine = new collection.mutable.MutableList[DocumentToken];
	    val rawLine = in.next()
	    totalLength += rawLine.length
	    if ( in.hasNext ) totalLength += 1
	    
	    for ( tkn <- rawLine.split("\\s") ) {
	        if ( tkn.length > 0 ) {
	        	parsedLine += StringDT( OriginalPosition(lineNumber, pos, tkn.length ),
	        	        				if ( tkn.contains("&") ){ unescapeHtml4(tkn) } else { tkn } )
	        }
	        pos += (tkn.length + 1)
	    }
	    parsedLine += LineBreakDT( OriginalPosition(lineNumber, pos, 1) )
	    
	    parsedLine
	}
	
	private def lastStringDtOpt: Option[StringDT] = {
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
	    lastStringDtOpt match {
	        case None => false
	        case Some(sdt) => explicitHyphenation.findFirstIn(sdt.text) match {
	            case None    => false
	            case Some(_) => true
	        }
	    }
	}

}


object DTS_Test extends App {
    val input = List( ("simple", Array("hello world this is line 1", "this is a line with &#39; escapes", "I'm a hyphen-","ated line, so need to", "see if that works" )),
            		  ("double-hyph", Array("this line conca-","tenates to the next li-","ne, which contac-","ts even more") ),
            		  ("end-hyph", Array("line line line-") ),
            		  ("false-hyph", Array("the following line ends with a '-'", "but shouldn't be de-hyphenated:","in the span 1998 -", " 1989 there was a thing") ),
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
	            case LineBreakDT(p) => println( "/line break/")
	            case EndOfFileDT(p) => go=false; println("/end")
	        }
	    }
    }   
}

