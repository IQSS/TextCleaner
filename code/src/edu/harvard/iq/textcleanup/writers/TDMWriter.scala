package edu.harvard.iq.textcleanup.writers;

import edu.harvard.iq.textcleanup.documentparser.UnWrappedDocument
import java.nio.file.Path
import java.nio.file.Files
import scala.actors.Actor
import scala.actors.Channel
import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import edu.harvard.iq.textcleanup.documentparser.UnWrappedDocument
import java.math.BigInteger
import edu.harvard.iq.textcleanup.documentparser.UnWrappedDocumentStats
import java.util.concurrent.ArrayBlockingQueue
import java.io.CharArrayWriter
import java.util.zip.ZipOutputStream
import java.io.FileOutputStream
import scala.collection.mutable.Buffer
import java.util.zip.ZipEntry

/**
 * An actor that aggregates {@link UnWrappedDocument}s until a certain it gets
 * to a threshold. Then it creates a sparse Text-Document Matrix, and writes it to disk.
 */
class TDMWriter( val termCountThreshold:Int, outputDirs:Traversable[Path], val shutdownHook:()=>Unit ) {
	
	private var flushesCount = 0
	private val docs = collection.mutable.Buffer[UnWrappedDocumentStats]()
	private val REPORT_INTERVAL = BigInteger.valueOf(10000)
	private val FILES_PER_FOLDER = 2000
	private val outputDirsBuffer = Buffer[Path]()
	
	private var fileCountInFolder = 0
	private var folderCount = 0
	outputDirsBuffer ++= outputDirs
	
	val queue = new ArrayBlockingQueue[UnWrappedDocumentStats](1024)
	
	def go() {
	    import edu.harvard.iq.textcleanup.Implicits._
	    new java.lang.Thread( ()=>{consumptionLoop} ).start()
	}
	
	def consumptionLoop  {

		var termCount = 0
		var docCount = BigInteger.ONE;
		
		var go=true
		while ( go ) {
			var docStat = queue.take()
			docStat match {
			    case UnWrappedDocumentStats( null, _ ) => {
			        go = false
			    }
				case doc@UnWrappedDocumentStats( p, c ) => {
					docs += doc
					termCount += doc.stats.size
					docCount = docCount.add(BigInteger.ONE)
					
					if ( termCount > termCountThreshold ) {
						flushStats()
						termCount=0
						docs.clear
	
					}
					if ( docCount.mod(REPORT_INTERVAL).equals(BigInteger.ZERO) ) {
						println( "- %s %s Documents processed".format(new java.util.Date, docCount) )
					}
				}
			}
		}
		flushStats()
		println( "- %s %s Documents processed".format(new java.util.Date, docCount) )
		if ( shutdownHook!=null ) shutdownHook()
	}
	
	private def flushStats() {
		flushesCount += 1
		
		val outFile = getOutputDir().resolve("pTDM_%04d.zip".format(flushesCount) )
		val zipStream = new ZipOutputStream( 
	            					new FileOutputStream(outFile.toFile()), StandardCharsets.UTF_8 )
		
		zipStream.putNextEntry( new ZipEntry("pTDM_%04d.txt".format(flushesCount)) )
	    val outPrinter = new PrintWriter( zipStream )
	    
		// map terms => num
	    val termList = docs.flatMap( _.stats.keys.map(k=>k.trim) ).toSet.
	    						filter( (k) => {!(k.isEmpty)} ).toSeq.sorted
	    termList.foreach( outPrinter.println(_) )
	    
	    outPrinter.println("==")
	    
	    // print TDM
	    val termToId:Map[String,Int] = termList.zipWithIndex.toMap;
	    docs.foreach( doc => {
	    	outPrinter.print( doc.originalPath.getFileName() )
	    	doc.stats.foreach( p => {
	    	    val id = termToId.getOrElse( p._1, -1 )
	    	    if ( id != -1 ) {
	    	    	outPrinter.print("\t" + id + (if (p._2>1) { "x" + p._2 } else {""} ) )
	    	    }
	    	})
	    	outPrinter.println()
	    }) 
	    
	    outPrinter.flush()
	    
	    zipStream.closeEntry()
	    zipStream.finish()
	    outPrinter.close()
	    fileCountInFolder += 1
	}
	
	def getOutputDir() = {
	    // get a non-full drive
	    var p1 = outputDirsBuffer(0)
	    while ( Files.getFileStore(p1).getUnallocatedSpace < (1024*1024*1.5).toInt ) {
	        outputDirsBuffer.remove(0)
	        fileCountInFolder = 0
	        folderCount = 0
	        p1 = outputDirsBuffer(0)
	    }
	    
	    if ( fileCountInFolder == FILES_PER_FOLDER ) {
	    	folderCount += 1
	    }
	    
	    var outputDir = p1.resolve( folderCount.toString )
	    if ( ! Files.exists(outputDir) ) {
	        Files.createDirectories(outputDir)
	        fileCountInFolder = 0
	    }
	    
	    outputDir
	}
	
}
