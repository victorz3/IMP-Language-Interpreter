package mx.unam.universaldist;

import java.io.BufferedReader;
import java.io.FileReader;
import java.sql.SQLException;
import java.util.InputMismatchException;
import java.util.Scanner;
import mx.unam.universaldist.db.Conn;
import mx.unam.universaldist.db.UniversalDistribution;
import mx.unam.universaldist.util.Util;

public class App 
{
    public static void main(String[] args)
    {
      	// Scanner for reading user input.
	Scanner sc = new Scanner(System.in);
	// UniversalDistribution object for interacting with the database.
	UniversalDistribution u = new UniversalDistribution();
	// boolean for ending the program's main loop.
	boolean endProgram = false;
	while(!endProgram){
	    System.out.println("What would you like to do?\n1.Add data to"
			       + "database.\n2.Make a query\n3.Leave.");
	    // User's option.
	    int option;
	    try{
		option = sc.nextInt();
	    }catch (InputMismatchException e){
		option = 3;
	    }
	    switch(option){
	    case 1: 
		System.out.println("Please enter the name of the outputs file");
		// Output file name.
		String filename = sc.next();
		try{
		    BufferedReader r = new BufferedReader(new FileReader(filename));
		    String l = r.readLine();
		    while(l != null){
			u.writeResult(l);
			l = r.readLine();
		    }
		}
		catch(Exception e){
		    System.err.println(e.getMessage());
		}finally{
		    Conn c = u.getConnection();
		    c.close();
		}
		break;
	    case 2:
		System.out.println("Please select a query:\n1.Number of halting"
				   + " programs\n2.Kolmogorov complexity for a"
				   + " string.\n3.Largest program that generates"
				   + " a string.\n4.Number of non-halting"
				   + " programs\n5.Number of programs that"
				   + " generate a string.\n6.Shortest program"
				   + " that generates a string.\n7.Universal"
				   + " distribution for a string.\n8.Check"
				   + " whether a program has been inserted to"
				   + "the database.\n9.Back to main menu.");
		try{
		    option = sc.nextInt();
		}catch (InputMismatchException e){
		    option = 9;
		}
		try{
		    switch(option){
		    case 1:
			System.out.println("The number of halting programs"
					   + " currently registered is: "
					   + u.getHaltingPrograms());
			break;
		    case 2:
			System.out.println("Please enter the string for which"
					   + " you want to check the Kolmogorov"
					   + " complexity. Enter 'epsilon' for the"
					   + " empty string.");
			// String for checking Kolmogorov complexity.
			String string = sc.next();
			break;
		    }
		} catch(SQLException e){
		    System.out.println(e.getMessage());
		    System.exit(1);
		}
	    }
	}
    }
}
