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
	
	val checkers = new collection.mutable.HashSet[SpellChecker]
	init()
	def init() {
	    val directory = new RAMDirectory()
		val idxWriterCfg = new IndexWriterConfig( LUCENE_43, new EnglishAnalyzer(LUCENE_43, CharArraySet.EMPTY_SET) )
		val dict = new IterableStringDictionary(words)
		
		for ( sd <- Array( new LevensteinDistance, new JaroWinklerDistance, new NGramDistance) ) {
		    checkers += new SpellChecker( directory, sd )
		}
		for ( chk <- checkers ) chk.indexDictionary( dict, idxWriterCfg, false )
	}
	
	def correct( word:String ) = {
	    
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
		        println("Can't correct: [%s]".format(word) )
		        word
		    } else {
		    	wordScore.maxBy( _._2 )._1
		    }
		}
	}
}


object TestCorrector extends App {
    val dict = Set[String]("hello","world","this","is","a","test","o'er")
    val corrector = new WordCorrector( 5, dict )
    
    for ( word <- Array("hello","hpllo","Ello","tis","this","thes", "o'er","over") ) {
        println("## " + word )
    	println( "## -> " + corrector.correct(word) )
    }
        
}