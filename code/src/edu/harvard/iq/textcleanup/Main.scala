package edu.harvard.iq.textcleanup
import java.io.File
import java.nio.file._
import scala.actors.Futures._
import scala.actors.Future
import java.util.concurrent._
import edu.harvard.iq.textcleanup.heuristics.SpellCheckersHeuristic

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
	import edu.harvard.iq.textcleanup.Utils.makeRunnable
	
	// TODO read from commandline using CliArgs
	val dictionaryPath = "/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/data/Dictionary.csv"
	val dataPath = "/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/data/70Election/"
	val outputPath = "/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/data-clean/"
	val maxCount = Integer.MAX_VALUE
	val workerThreadCount = 6
	
	println( "Document Cleanup" )
	println( "Version 0.5" )
	
	println( "Using dictionary:" )
	println( "\t" + dictionaryPath )
	val dictionary = new CsvDictionary( dictionaryPath )
  	println("dictionary has %,d words".format(dictionary.words.size) )
	
	val dataFilePath = FileSystems.getDefault.getPath( dataPath )
	val correctors = new ThreadLocal[WordCorrector] {
	    override def initialValue() = {
	        println("Inited dictionary")
	        new WordCorrector(5, dictionary.words )
	    }
	}
	
	val executorSvc = if ( workerThreadCount == 1 ) Executors.newSingleThreadExecutor() 
						else Executors.newFixedThreadPool(workerThreadCount)
						
	var counter = 0l;
	Files.walkFileTree(dataFilePath,
		new SimpleFileVisitor[Path] {
	    	override def visitFile( f:Path, attrs: attribute.BasicFileAttributes ) = {
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
	    		if ( counter >= maxCount ) FileVisitResult.TERMINATE else FileVisitResult.CONTINUE
	    	}
	})
		
	println( "Submitted %d files".format( counter ) )
	
	executorSvc.shutdown()
	executorSvc.awaitTermination(3, java.util.concurrent.TimeUnit.HOURS)
	
}
