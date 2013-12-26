package edu.harvard.iq.textcleanup

import java.nio.file.Paths
import java.util.Date
import edu.harvard.iq.textcleanup.writers.TDMWriter
import java.util.concurrent.ArrayBlockingQueue
import edu.harvard.iq.textcleanup.documentparser.RawDocument
import edu.harvard.iq.textcleanup.documentparser.UnWrappedDocument
import edu.harvard.iq.textcleanup.documentparser.UnWrappedDocumentStats

/**
 * Stages and executes the unwrapping of files in a directory
 */
object UnWrapMain extends App {
    
    val params = new CliArgs( args )
    
    val inputRoot = Paths.get(params.files(0))
    val outputRoot = Paths.get(params.files(1))
    val dictionaryRoot = Paths.get(params.files(2))
    val tdmRoot = params.files.drop(3).map( Paths.get(_) )
    
    val workerCount = params.values.get("worker-count") match {
        case Some(s) => s.toInt
        case None    => (Runtime.getRuntime().availableProcessors()*1.5).toInt
    }
    val queueLength = params.values.get("queue-length") match {
        case Some(s) => s.toInt
        case None    => 1024
    }
    val tdmSize = params.values.get("tdm-size") match {
        case Some(s) => s.toInt
        case None    => 1000
    }
    
    println("TextCleaner: Unwrapping")
    println("[multi-stage-version 195d9b2]")
    println( new Date )
    println( "input:\t%s".format(inputRoot) )
    println( "output:\t%s".format(outputRoot) )
    println( "dictionary:\t%s".format(dictionaryRoot) )
    println( "tdms:\t%s".format(tdmRoot) )
    println( "options: " )
    println( "worker count:\t %d".format(workerCount) )
    println( "queue length:\t %d".format(queueLength) )
    println( "tdm size:\t %d".format(tdmSize) )
    
    println()
    print("loading dictionary...")
    val words = new LineWordSetLoader().load( dictionaryRoot )
    println("DONE (%,d words)".format(words.size) )
    
    val tdmWriter = new TDMWriter(tdmSize, tdmRoot, ()=>{println("%s ALL DONE!".format(new Date))});
    val docQueue = new ArrayBlockingQueue[RawDocument](queueLength)
    val workers = collection.mutable.Set[DocumentUnWrapper]()
    for ( i <- 0 to workerCount ) {
        val duw = new DocumentUnWrapper( words, outputRoot )
        workers += duw
        duw.go(docQueue, tdmWriter)
    }
    tdmWriter.go()
    
    new DirectoryReader( inputRoot, docQueue, ()=>{
        println("Done queueing files");
        for ( i <- 0 to workerCount ) {
            docQueue.put( DocumentUnWrapper.END_OF_INPUT ) // tell everyone we're done
        }
        
        workers.foreach( w => {
            w.waitForTermination
        })
        println("All workers done")
        
        // shutdown signal
        tdmWriter.queue.put( new UnWrappedDocumentStats(null,null) )


    }).go
    
    println( "all started" )
}