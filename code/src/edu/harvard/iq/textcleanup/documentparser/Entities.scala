package edu.harvard.iq.textcleanup.documentparser

/**
 * The original position in the file. Used to track the changes and debug the heuristics.
 */
class OriginalPosition( val line:Int, val start:Int, val length:Int ) {
    override def toString = "[pos line:%d start:%d length:%d]".format( line, start, length)
}

object OriginalPosition {
    def apply( line:Int, start:Int, length:Int ) = new OriginalPosition( line, start, length )
}

/**
 * Base class for what the document scanner emits to the application.
 */
sealed abstract class DocumentToken {
    val pos:OriginalPosition
}
case class LineBreakDT(pos:OriginalPosition) extends DocumentToken

case class EndOfFileDT(pos:OriginalPosition) extends DocumentToken

case class StringDT(pos:OriginalPosition, text:String ) extends DocumentToken { 
    def mergeForward( next:StringDT ) = {
        StringDT( OriginalPosition(pos.line, 
                					pos.start, 
                					pos.length + next.pos.length + (if(pos.line != next.pos.line) next.pos.start else 0) ),
                	text + next.text);
    }
    
    /**
     * The part of the token, from the first letter to the last (inclusive).
     */
    def makeTextCore = text.dropWhile( ! Character.isLetter(_) ).reverse.dropWhile( !Character.isLetter(_) ).reverse
    
    private var _textCore:(()=>String) = { 
        val c=text.dropWhile( ! Character.isLetter(_) ).reverse.dropWhile( !Character.isLetter(_) ).reverse
        _textCore = ()=>{c}
        _textCore
       }
    
    def textCore = _textCore()
}


object EntitiesTest extends App {
    val sdt1 = StringDT( OriginalPosition(1,2,10), "!!Hello!!!")
    println( sdt1.textCore )
    println( sdt1.textCore )
    println( sdt1.textCore )
}