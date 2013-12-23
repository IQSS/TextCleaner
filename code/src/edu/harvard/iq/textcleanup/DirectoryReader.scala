package edu.harvard.iq.textcleanup

import java.util.concurrent.BlockingQueue
import java.nio.file.Path
import edu.harvard.iq.textcleanup.documentparser.RawDocument
import java.nio.file._
import edu.harvard.iq.textcleanup.documentparser.RawDocument
import java.nio.charset.StandardCharsets

/**
 * Gets a directory, pushes all the text files there (recursively)
 * as {@link RawDocuemnt}s, to a queue. The paths of the generated
 * {@code RawDocument}s are relative to the root.
 */
class DirectoryReader( val root:Path, val queue:BlockingQueue[RawDocument], done:()=>Unit ){
	import Implicits.makeRunnable
	
    def go() {
        new java.lang.Thread( ()=>{descend(root)} ).start()
    }
    
    def descend( p:Path ) {
    	Files.walkFileTree(root,
		new SimpleFileVisitor[Path] {
	    	override def visitFile( f:Path, attrs: attribute.BasicFileAttributes ) = {
	    		if ( f.getFileName().toString.trim.toLowerCase().endsWith("txt") ) {
	    			queue.put( RawDocument(root.relativize(f), new String(Files.readAllBytes(f), StandardCharsets.UTF_8) ) )
			    }
	    		FileVisitResult.CONTINUE
	    	}
    	})
    	
    	done()
    }
}