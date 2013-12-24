package edu.harvard.iq.textcleanup.writers;

import edu.harvard.iq.textcleanup.documentparser.UnWrappedDocument
import java.nio.file.Path
import scala.actors.Actor
import scala.actors.Channel
import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import edu.harvard.iq.textcleanup.documentparser.UnWrappedDocument
import java.math.BigInteger
import edu.harvard.iq.textcleanup.documentparser.UnWrappedDocumentStats
import java.util.concurrent.ArrayBlockingQueue

/**
 * An actor that aggregates {@link UnWrappedDocument}s until a certain it gets
 * to a threshold. Then it creates a sparse Text-Document Matrix, and writes it to disk.
 */
class TDMWriter( val termCountThreshold:Int, val outputDir:Path, val shutdownHook:()=>Unit ) {

	private var flushesCount = 0
	private val docs = collection.mutable.Buffer[UnWrappedDocumentStats]()
	private val REPORT_INTERVAL = BigInteger.valueOf(10000)
	
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
	    
	    val outFile = outputDir.resolve("pTDM_%04d.txt".format(flushesCount) )
	    val out = new PrintWriter(java.nio.file.Files.newBufferedWriter(outFile,StandardCharsets.UTF_8))
	    
		// map terms => num
	    val termList = docs.flatMap( _.stats.keys.map(k=>k.trim) ).toSet.
	    						filter( (k) => {!(k.isEmpty)} ).toSeq.sorted
	    termList.foreach( out.println(_) )
	    
	    out.println("==")
	    
	    // print TDM
	    val termToId:Map[String,Int] = termList.zipWithIndex.toMap;
	    docs.foreach( doc => {
	    	out.print( doc.originalPath.getFileName() )
	    	doc.stats.foreach( p => {
	    	    val id = termToId.getOrElse( p._1, -1 )
	    	    if ( id != -1 ) {
	    	    	out.print("\t" + id + (if (p._2>1) { "x" + p._2 } else {""} ) )
	    	    }
	    	})
	    	out.println()
	    }) 
	    
	    out.close()
	}

}
