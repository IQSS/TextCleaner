package edu.harvard.iq.textcleanup

/**
 * A class that helps making sense of command-line agruments.
 * Assumes the format is :
 * <code>
 * appname -switch1 -switch2 --key1=value1 --key2=value2 file1 file2 file3...
 * 
 * </code>
 */
class CliArgs( lines:Iterable[String] ) {
	
	val files    = lines.filter( ! _.startsWith("-") );
    val switches = Set(lines.filter( (s) => {s.startsWith("-") && ! s.startsWith("--")}) );
    val values   = lines.filter( _.startsWith("--") ).
    						flatMap( (s)=>{ List(s.drop(2).split("=")) } ).
    							flatMap( a => Array((a(0),a(1))) ).toMap
    
}