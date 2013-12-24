package edu.harvard.iq.textcleanup.writers;

import edu.harvard.iq.textcleanup.documentparser.UnWrappedDocument
import java.nio.file.Path
import scala.actors.Actor
import scala.actors.Channel
import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import edu.harvard.iq.textcleanup.documentparser.UnWrappedDocument
import java.math.BigInteger

/**
 * An actor that aggregates {@link UnWrappedDocument}s until a certain it gets
 * to a threshold. Then it creates a sparse Text-Document Matrix, and writes it to disk.
 */
class TDMWriter( val termCountThreshold:Int, val outputDir:Path, val shutdownHook:()=>Unit ) extends Actor {

	private var flushesCount = 0
	private val docs = collection.mutable.Buffer[UnWrappedDocument]()
	private val REPORT_INTERVAL = BigInteger.valueOf(10000)

	override def act  {

		var termCount = 0
		var docCount = BigInteger.ONE;
		while ( true ) {
			receive {
			    case UnWrappedDocument( null, _ ) => {
			        flushStats()
			        if ( shutdownHook!=null ) shutdownHook()
			        exit()
			    }
				case doc@UnWrappedDocument( p, c ) => {
					docs += doc
					termCount += doc.stats.size
					docCount = docCount.add(BigInteger.ONE)
					
					if ( termCount > termCountThreshold ) {
						flushStats()
						termCount=0
						docs.clear

					}
					if ( docCount.mod(REPORT_INTERVAL).equals(BigInteger.ZERO) ) {
						println( "- %s Documents processed".format(docCount) )
					}
				}
			}
		}
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
	    	    	out.print("\t" + id + "x" + p._2 )
	    	    }
	    	})
	    	out.println()
	    }) 
	    
	    out.close()
	}

}
