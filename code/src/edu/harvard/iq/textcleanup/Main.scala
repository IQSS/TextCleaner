package edu.harvard.iq.textcleanup
import java.io.File
import java.nio.file._
import scala.actors.Futures._
import scala.actors.Future

object Utils {
	// usage: iles.walkFileTree(dir.toPath, (f: Path) => println(f))
    
	implicit def makeFileVisitor(f: (Path) => Any) = new SimpleFileVisitor[Path] {
		override def visitFile(p: Path, attrs: attribute.BasicFileAttributes) = { 
			f(p)
			FileVisitResult.CONTINUE 
		}
	}
}

object Main extends App {
	import edu.harvard.iq.textcleanup.Utils.makeFileVisitor
	val dictionaryPath = "/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/data/Dictionary.csv"
	val dataPath = "/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/data/70Election/"
	val outputPath = "/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/data-clean/"
    
	println( "Document Cleanup" )
	println( "Version 0.5" )
	
	println( "Using dictionary:" )
	println( "\t" + dictionaryPath )
	val dictionary = new CsvDictionary( dictionaryPath )
	
  	dictionary.init
  	println("dictionary has %,d words".format(dictionary.words.size) )
	
//	val corrector = new WordCorrector(5, dictionary.words )
	val dataFilePath = FileSystems.getDefault.getPath( dataPath )
	val correctors = new ThreadLocal[WordCorrector] {
	    override def initialValue() = {
	        println("Inited dictionary")
	        new WordCorrector(5, dictionary.words )
	    }
	}
	
	val tasks = new collection.mutable.Queue[Future[Any]]
	
	Files.walkFileTree(dataFilePath, (f:Path) => {
		if ( f.getFileName().toString.trim.endsWith("txt") ) {
		    tasks += future {
					    	val outFilePath = FileSystems.getDefault.getPath( outputPath ).resolve( f.getFileName() )
							val docClean = new DocumentCleaner(f, outFilePath, correctors.get )
							docClean.go
							println( f.getFileName() + " done" )
					    	()
					    }
	    } 
	})
	println( "Scheduled %d tasks".format(tasks.size) )
	scala.actors.Futures.awaitAll(60*60*1000, tasks: _*)
	
}
