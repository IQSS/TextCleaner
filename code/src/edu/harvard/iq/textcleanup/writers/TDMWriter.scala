package edu.harvard.iq.textcleanup.writers;

import edu.harvard.iq.textcleanup.documentparser.UnWrappedDocument
import java.nio.file.Path
import scala.actors.Actor
import scala.actors.Channel
import java.io.PrintWriter
import java.nio.charset.StandardCharsets
import edu.harvard.iq.textcleanup.documentparser.UnWrappedDocument

/**
 * An actor that aggregates {@link UnWrappedDocument}s until a certain it gets
 * to a threshold. Then it creates a sparse Text-Document Matrix, and writes it to disk.
 */
class TDMWriter( val termCountThreshold:Int, val outputDir:Path ) extends Actor {

	private var flushesCount = 0
	private val docs = collection.mutable.Buffer[UnWrappedDocument]()

	override def act  {

		var termCount = 0

		while ( true ) {
			receive {
			    case UnWrappedDocument( null, _ ) => {
			        flushStats()
			        println("TDM actor exiting")
			        exit()
			    }
				case doc@UnWrappedDocument( p, c ) => {
					docs += doc
					termCount += doc.stats.size

					if ( termCount > termCountThreshold ) {
						flushStats()
						termCount=0
						docs.clear

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
