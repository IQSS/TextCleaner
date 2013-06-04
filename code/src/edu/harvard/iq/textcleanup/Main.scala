package edu.harvard.iq.textcleanup
import java.io.File
import java.nio.file._
import scala.actors.Futures._
import scala.actors.Future
import java.util.concurrent._

object Utils {
	// usage: iles.walkFileTree(dir.toPath, (f: Path) => println(f))
    
	implicit def makeFileVisitor(f: (Path) => Any) = new SimpleFileVisitor[Path] {
		override def visitFile(p: Path, attrs: attribute.BasicFileAttributes) = { 
			f(p)
			FileVisitResult.CONTINUE 
		}
	}
	
	implicit def makeRunnable( f:()=>Any ) = new Runnable {
	    override def run() = f() 
	}
}

object Main extends App {
	import edu.harvard.iq.textcleanup.Utils.makeFileVisitor
	import edu.harvard.iq.textcleanup.Utils.makeRunnable
	
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
	
	val executorSvc = Executors.newFixedThreadPool(6)
	var counter = 0l;
	Files.walkFileTree(dataFilePath, (f:Path) => {
		if ( f.getFileName().toString.trim.endsWith("txt") ) {
		    counter += 1
		    executorSvc.submit(
		       () => {
			    	val outFilePath = FileSystems.getDefault.getPath( outputPath ).resolve( f.getFileName() )
					val docClean = new DocumentCleaner(f, outFilePath, correctors.get )
					docClean.go
					println( f.getFileName() + " done" )
			    })
	    } 
	})
	
	println( "Submitted %d files".format( counter ) )
	
	executorSvc.shutdown()
	executorSvc.awaitTermination(3, java.util.concurrent.TimeUnit.HOURS)
	
}
