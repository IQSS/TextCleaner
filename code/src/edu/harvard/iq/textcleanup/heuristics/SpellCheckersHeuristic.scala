package edu.harvard.iq.textcleanup.heuristics
import org.apache.lucene.search.spell.SpellChecker
import org.apache.lucene.store.RAMDirectory
import org.apache.lucene.search.spell._
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.analysis.util.CharArraySet
import org.apache.lucene.analysis.en.EnglishAnalyzer
import org.apache.lucene.util.Version.LUCENE_43
import collection.mutable.Map
import scala.collection.mutable.HashMap
import edu.harvard.iq.textcleanup.IterableStringDictionary
import scala.Array.canBuildFrom

/**
 * This heuristic consults the three built-in Lucene spellcheckers,
 * and finds the best suggestion based on weighted majority vote.
 * Each spell checker gives up to 
 */
class SpellCheckersHeuristic( val dist:Int, words:collection.Set[String] ) extends Heuristic {
	
    val checkers = new collection.mutable.HashSet[SpellChecker]
	init( words )
	
	/** initing the system */
	private[this] def init( words:collection.Set[String] ) {
	    val directory = new RAMDirectory()
		val idxWriterCfg = new IndexWriterConfig( LUCENE_43, new EnglishAnalyzer(LUCENE_43, CharArraySet.EMPTY_SET) )
		val dict = new IterableStringDictionary(words)
		
		for ( sd <- Array( new LevensteinDistance, new JaroWinklerDistance, new NGramDistance) ) {
		    checkers += new SpellChecker( directory, sd )
		}
		for ( chk <- checkers ) chk.indexDictionary( dict, idxWriterCfg, false )
	}
	
	/**
	 * Corrects lower case words.
	 */
	 override def suggest( word:String )  = {
	    val wordScore:Map[String,Int] = new HashMap()
		
	    for ( checker <- checkers ) {
		  	val options = checker.suggestSimilar( word, dist );
		  	for ( (word,score) <- options.zip( (0 until dist).reverse ) ) {
		  	    wordScore(word) = wordScore.get(word) match {
		  	    	case None            => score
		  	    	case Some(prevScore) => prevScore+score 
		  	    }
		  	}
	  	}
	    
	    if ( wordScore.isEmpty ) {
	        None
	    } else {
	    	Some( wordScore.maxBy( _._2 )._1 )
	    }
	}

}


object TestCorrector extends App {
    val dict = Set[String]("hello","world","this","is","a","test","o'er")
    val corrector = new SpellCheckersHeuristic( 5, dict )
    
    for ( word <- Array("hello","hpllo","Ello","tis","this","thes", "o'er","over", "Hello", "HELLO", "Pello", "PELLO", "PeLLo") ) {
        println("## " + word )
    	println( "## -> " + corrector.suggest(word) )
    }
        
}