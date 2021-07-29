package mx.unam.universaldist;

import java.io.BufferedReader;
import java.io.FileReader;

public class App 
{
    public static void main( String[] args )
    {
	// UniversalDistribution u = new UniversalDistribution();
    // 	try{
    // 	    BufferedReader r = new BufferedReader(new FileReader("outputs2.txt"));
    // 	    String l = r.readLine();
    // 	    while(l != null){
    // 		u.writeResult(l);
    // 		l = r.readLine();
    // 	    }
    // 	}
    // 	catch(Exception e){
    // 	    System.err.println(e.getMessage());
    // 	}finally{
    // 	    Conn c = u.getConnection();
    // 	    c.close();
    // 	}
    // }
	UniversalDistribution u = new UniversalDistribution();
	int x = u.numLength(20, 255);
	int y = u.numLength(0, 255);
	int w = u.numLength(1000000000, 255);
	System.out.printf("Resultados de las pruebas\n Prueba 1:%d\n Prueba" +
			  "2:%d\nPrueba 3:%d\n", x, y, w);
    }
}
