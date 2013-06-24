package edu.harvard.iq.textcleanup.heuristics

import edu.harvard.iq.textcleanup.WordCorrector
import java.lang.Character.isLetter

/**
 * This heuristic keeps the non-letters, and tries to fix the letter spans.
 * So "hekko," would be looked as [hekko][,], and "lkj987qeqw--3oij" as [lkj][987][qeqw][--3][oij]
 */
class TextCoreSpellCheckHeuristic( val corrector:WordCorrector) extends Heuristic {
	
    override def suggest( token:String ) : Option[String] = {
        if ( token.forall(isLetter(_)) ) return None
        
        val partList = token.foldLeft[List[String]](Nil)( (l,c) => l match {
            case Nil => c.toString :: Nil
            case   _ => if ( isLetter(l.last.last)==isLetter(c))
        					l.dropRight(1) :+ l.last+c
        				else l :+ c.toString
        })
        
        val fixList = partList.map( part =>
        	if ( isLetter(part(0)) ) corrector.correct(part) match {
        	    case Nil    => part
        	    case b :: r => b.suggestion 
        	} else {
        	    part
        	}
        )
        
        Some(fixList.mkString)
    }
    
    override def toString = this.getClass.getName.split("\\.").last
    
}