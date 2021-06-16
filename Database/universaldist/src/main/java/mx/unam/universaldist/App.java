package mx.unam.universaldist;

import java.io.BufferedReader;
import java.io.FileReader;

public class App 
{
    public static void main( String[] args )
    {	
	try{
	    BufferedReader r = new BufferedReader(new FileReader("outputs.txt"));
	    String l = r.readLine();
	    UniversalDistribution u = new UniversalDistribution();
	    while(l != null){
		u.writeResult(l);
		l = r.readLine();
	    } 
	}
	catch(Exception e){
	    System.err.println(e.getMessage());
	}
    }
}
