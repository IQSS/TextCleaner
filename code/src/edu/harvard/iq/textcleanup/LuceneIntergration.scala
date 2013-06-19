package edu.harvard.iq.textcleanup
import java.util.Comparator
import org.apache.lucene.search.spell.{Dictionary => LuceneDictionary }
import org.apache.lucene.util.BytesRefIterator
import org.apache.lucene.util.BytesRef

/**
 * All one needs to load a CSV file and use it as a dictionary in Lucene
 */
class CsvDictionary( val filePath:String ) {
	
  private val wordSet = init()
  def words = wordSet
  
  private[this] def init() = {
    import scala.io.Source
    val src = Source.fromFile( filePath, "UTF-8" )
    val workingDict = new collection.mutable.TreeSet[String]
    for ( l <- src.getLines ) {
    	var line = l.trim()
    	if ( line.startsWith("\"") ) line = line.drop(1)
    	if ( line.endsWith("\"") ) line = line.dropRight(1)
    	workingDict += line.toLowerCase
    }
    src.close()
    workingDict.toSet[String]
  }
  
}

class IterableStringDictionary(val words:Iterable[String]) extends LuceneDictionary {
	
	def getWordsIterator : BytesRefIterator = {
		new StringByteRefIterator( words )
	}
}

class StringByteRefIterator( words:Iterable[String] ) extends BytesRefIterator {
	val itr = words.iterator
	
	def next:BytesRef = {
	  if ( itr.hasNext ) {
	    new BytesRef( itr.next.getBytes )
	  } else {
	    null
	  }
	}
  def getComparator:Comparator[BytesRef] = null
}
