package edu.harvard.iq.textcleanup
import java.io.File
import java.nio.file._

object Main extends App {
	val dictionaryPath = "/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/data/Dictionary.csv"
	val dataPath = "/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/data/70Election/1.txt"
	val outputPath = "/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/data-clean/"
    
	println( "Document Cleanup" )
	println( "Version 0.5" )
	
	println( "Using dictionary:" )
	println( "\t" + dictionaryPath )
	val dictionary = new CsvDictionary( dictionaryPath )
	
  	dictionary.init
  	println("dictionary has %,d words".format(dictionary.words.size) )
	
	val corrector = new WordCorrector(5, dictionary.words )
	val dataFilePath = FileSystems.getDefault.getPath( dataPath )
	val outFilePath = FileSystems.getDefault.getPath( outputPath ).resolve( dataFilePath.getFileName() )
	
	println( "cleaning %s".format(dataFilePath) )
	println( "into %s".format(outFilePath) )
	val start = System.currentTimeMillis()
	val docClean = new DocumentCleaner(dataFilePath, outFilePath, corrector)
	docClean.go
	val end = System.currentTimeMillis()
	println( "done in %d msec".format(end-start) )
}

object Utils {
	// usage: iles.walkFileTree(dir.toPath, (f: Path) => println(f))
    
	implicit def makeFileVisitor(f: (Path) => Unit) = new SimpleFileVisitor[Path] {
		override def visitFile(p: Path, attrs: attribute.BasicFileAttributes) = { 
			f(p)
			FileVisitResult.CONTINUE 
		}
	}
}