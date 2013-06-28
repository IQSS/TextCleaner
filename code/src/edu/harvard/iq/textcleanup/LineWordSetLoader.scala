package edu.harvard.iq.textcleanup

import java.nio.file.Path
import java.nio.file.FileSystems
import java.nio.file.Files

/** 
 *  Loads a set of words from multiple files. Assumes one word per line.
 *  Words are changed to lowercase.
 */

class LineWordSetLoader() {
    
    /**
     * Given a Path, loads all the .txt files there.
     */
    def load( path:Path ):Set[String] = {
   	    import Utils.makeFileVisitor
	    val files = collection.mutable.Set[Path]()
	    Files.walkFileTree(path, (f:Path) => if (f.getFileName.toString.endsWith(".txt") ) files += f  )
	    load( files.toSet )
    }
    
    def load( files:Set[Path] ) : Set[String] = {
    	files.par.flatMap( loadSingleFile(_) ).to[Set]
    }
    
    private def loadSingleFile( p:Path ) = {
    	val src = scala.io.Source.fromFile( p.toFile(), "UTF-8" )
        try {
        	src.getLines()
        		.map( l => l.trim.toLowerCase ).toSet
        } finally {
            src.close
        }
    }
    
}

object LineWordsetLoaderText extends App {
    val p = FileSystems.getDefault.getPath("/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/dict/")
    val loader = new LineWordSetLoader()
    val words = loader.load( p )
    println( "words has %,d words".format(words.size) )
}