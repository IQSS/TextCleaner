package edu.harvard.iq.textcleanup
import java.io.File
import java.nio.file._
import scala.actors.Futures._
import scala.actors.Future
import java.util.concurrent._
import edu.harvard.iq.textcleanup.heuristics.SpellCheckersHeuristic
import edu.harvard.iq.textcleanup.writers.TextDocumentStatisticsWriter
import edu.harvard.iq.textcleanup.heuristics.TextCoreSpellCheckHeuristic
import edu.harvard.iq.textcleanup.heuristics.WordUnmergerHeuristic

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
	val VERSION="0.5"
	    
	// TODO read from commandline using CliArgs
	val dictionaryPath = "/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/data/Dictionary.csv"
	val dataPath = "/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/data/70Election/"
	val outputPath = "/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/data-clean/"
	val statisticsFolderPath = "/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/statistics/"
	val maxCount = 1 //Integer.MAX_VALUE
	val workerThreadCount = 6
	val SPELL_CHECKER_DIST = 5
	
	
	println( "Document Cleanup" )
	println( "Version 0.5" )
	
	println( "Using dictionary:" )
	println( "\t" + dictionaryPath )
	val dictionary = new CsvDictionary( dictionaryPath )
  	println("dictionary has %,d words".format(dictionary.words.size) )
	
	val dataFilePath = FileSystems.getDefault.getPath( dataPath )
	val statisticsPath = FileSystems.getDefault.getPath( statisticsFolderPath )
	val correctors = new ThreadLocal[WordCorrector] {
	    override def initialValue() = {
	        // TODO make this the document cleaner, not just the corrector.
	        println("Inited dictionary")
	        val wc = new WordCorrector()
	        wc.add( new SpellCheckersHeuristic( SPELL_CHECKER_DIST, dictionary.words) )
	        wc.add( new TextCoreSpellCheckHeuristic(wc) )
	        wc.add( new WordUnmergerHeuristic(dictionary.words) )
	        wc
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
							val docCleaner = new DocumentCleaner( dictionary.words, correctors.get )
							val stats = docCleaner.go( f, outFilePath )
							val tdsp = new TextDocumentStatisticsWriter( statisticsPath )
					    	tdsp.write(stats)
							println( stats )
					    })
			    }
	    		if ( counter >= maxCount ) FileVisitResult.TERMINATE else FileVisitResult.CONTINUE
	    	}
	})
		
	println( "Submitted %d files".format( counter ) )
	
	executorSvc.shutdown()
	executorSvc.awaitTermination(3, java.util.concurrent.TimeUnit.HOURS)
	
}
