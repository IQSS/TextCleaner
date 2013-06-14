package edu.harvard.iq.textcleanup.heuristics

import scala.math.min
import scala.math.max
 
object StringDistance {
	/**
	 * Based on Matt Malone (http://oldfashionedsoftware.com/2009/11/19/string-distance-and-refactoring-in-scala/)
	 */
	def levenshtein(s1: String, s2: String): Int = {
	    def minimum(i1: Int, i2: Int, i3: Int) = min(min(i1, i2), i3)
	 
	    val dist = Array.ofDim[Int](s1.length + 1, s2.length + 1)
	 
	    for (idx <- 0 to s1.length) dist(idx)(0) = idx
	    for (jdx <- 0 to s2.length) dist(0)(jdx) = jdx
	 
	    for (idx <- 1 to s1.length; jdx <- 1 to s2.length)
	      dist(idx)(jdx) = minimum (
	        dist(idx-1)(jdx  ) + 1,
	        dist(idx  )(jdx-1) + 1,
	        dist(idx-1)(jdx-1) + (if (s1(idx-1) == s2(jdx-1)) 0 else 1) )
	 
	    dist(s1.length)(s2.length)
	}
}