package mx.unam.universaldist.util;

import java.util.regex.Pattern;

/** 
 * Class for utility functions.
 * @author Victor Zamora
 */
public class Util{

    /**
     * Checks whether a String is one of:
     * An element of {0, 1}+
     * The String "epsilon"
     * The String "ε"
     * @param s - String to check.
     * @return true if the String is one of the aforementioned, false
     * otherwise. 
     */
    public static boolean isBinaryString(String s){
	// Binary string pattern.
	return s.equals("epsilon") || Pattern.matches("[01]+|ε", s);
    }
}
