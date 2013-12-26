package edu.harvard.iq.textcleanup

import java.io.File

object LogParser extends App {
	
    
    
    val src = io.Source.fromFile( new File("/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/statistics/run-log.log") )
    var timings = src.getLines.filter( _.startsWith("-") )
    		    .map( _.split(" ")(4) )
    			.map( time => {
    			    var tc = time.split(":").map( _.toInt )
    			    tc(2)+tc(1)*60+tc(0)*360} )
    var diffs = timings.grouped(2)
    					.filter( _.size==2 )
    					.map( p=>p(1)-p(0) ).foreach( println(_) )
    			
}