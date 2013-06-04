package edu.harvard.iq.textcleanup
import org.apache.lucene.search.spell.SpellChecker
import org.apache.lucene.store.RAMDirectory
import org.apache.lucene.search.spell._
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.analysis.util.CharArraySet
import org.apache.lucene.analysis.en.EnglishAnalyzer
import org.apache.lucene.util.Version.LUCENE_43
import collection.mutable.Map
import scala.collection.mutable.HashMap

/**
 * Takes a word, that's suspected to be wrong,
 * and returns a corrected version of it.
 * This class consults an explicit word dictionary,
 * and some lucene dictionaries to get a good guess at the correct word.
 * Words that are in the explicit dictionary are assumed to be correct.
 * Otherwise we do a majority vote between the dictionaries.
 */
class WordCorrector( val dist:Int, val words:collection.Set[String] ) {
	
    var uncorrectableWords = 0l;
	val checkers = new collection.mutable.HashSet[SpellChecker]
	init()
	
	/** initing the system */
	private[this] def init() {
	    val directory = new RAMDirectory()
		val idxWriterCfg = new IndexWriterConfig( LUCENE_43, new EnglishAnalyzer(LUCENE_43, CharArraySet.EMPTY_SET) )
		val dict = new IterableStringDictionary(words)
		
		for ( sd <- Array( new LevensteinDistance, new JaroWinklerDistance, new NGramDistance) ) {
		    checkers += new SpellChecker( directory, sd )
		}
		for ( chk <- checkers ) chk.indexDictionary( dict, idxWriterCfg, false )
	}
	
	/**
	 * Corrects words. Word can be lowercase or upper case,
	 * which means they get converted to lowercase, corrected
	 * and have their case restored. <br />
	 * Case restoration is as follows:
	 * <ul>
	 * 	<li>upper case only &rarr; upper case</li>
	 *  <li>title case &rarr; title case</li>
	 *  <li>others (e.g. uppercase in the middle of a word) &rarr; lowercase</li>
	 * </ul>
	 */
	def correct( word:String ) = {
	    if ( isLowerCaseOnly(word) ) {
	        lowercaseCorrect( word )
	    } else {
	        val corrected = lowercaseCorrect( word.toLowerCase() )
	        if ( isUpperCaseOnly(word) ) {
	            corrected.toUpperCase()
	        } else {
	            if ( isTitleCase(word) ) 
	                corrected.substring(0,1).toUpperCase + corrected.substring(1)
	            else 
	                corrected
	        }
	    } 
		
	}
	
	/**
	 * Corrects lower case words.
	 */
	private def lowercaseCorrect( word:String )  = {
		if ( words.contains(word) ) {
			word
		} else {
		    var wordScore:Map[String,Int] = new HashMap()
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
		        uncorrectableWords += 1
		        word
		    } else {
		    	wordScore.maxBy( _._2 )._1
		    }
		}
	}
	
	def uncorrectables = uncorrectableWords
	
	def reset() {
	    uncorrectableWords=0
	}
	
	def isLowerCaseOnly( s:String ):Boolean = {
	    for ( c <- s.toCharArray) {
	      if ( c<'a' || c>'z' ) return false
	    }
	    true
	}
	
	def isUpperCaseOnly( s:String ):Boolean = {
	    for ( c <- s.toCharArray) {
	      if ( c<'A' || c>'Z' ) return false
	    }
	    true
	}
	def isTitleCase( s:String ) =  {
	    isUpperCaseOnly( s.substring(0,1) ) && isLowerCaseOnly( s.substring(1) )
	}
}


object TestCorrector extends App {
    val dict = Set[String]("hello","world","this","is","a","test","o'er")
    val corrector = new WordCorrector( 5, dict )
    
    for ( word <- Array("hello","hpllo","Ello","tis","this","thes", "o'er","over", "Hello", "HELLO", "Pello", "PELLO", "PeLLo") ) {
        println("## " + word )
    	println( "## -> " + corrector.correct(word) )
    }
        
}