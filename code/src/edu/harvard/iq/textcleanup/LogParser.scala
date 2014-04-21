package edu.harvard.iq.textcleanup

import java.io.File

object LogParser extends App {
	
	val dateFormat = new java.text.SimpleDateFormat( "E MMM d HH:mm:ss z y" )
    val src = io.Source.fromFile( new File("/Users/michael/Documents/Msc/IQSS/general/historical-text-cleanup/statistics/run-log.log") )
    var timings = src.getLines.filter( _.startsWith("-") )
    		    .map( _.substring(2, 30) )
    			.map( dateFormat.parse(_).getTime() )
    			.map( _/1000 ).toSeq
    
    println( "%d Documents per second".format( 5240169/(timings.last - timings(0)) ) )
//    var diffs = timings.grouped(2)
//    					.filter( _.size==2 )
//    					.map( p=>p(1)-p(0) ).foreach( println(_) )
    			
}