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
	while(true){
	    System.out.println("What would you like to do?\n1.Add data to"
			       + " database.\n2.Make a query\n3.Leave.");
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
				   + " the database.\n9.Back to main menu.");
		try{ 
		    option = sc.nextInt();
		}catch (InputMismatchException e){
		    option = 9;
		}
		try{
		    // Whether the user's input string is in binary.
		    boolean binary = false;
		    switch(option){
		    case 1:
			System.out.println("The number of halting programs"
					   + " currently registered is: "
					   + u.getHaltingPrograms() + ".");
			break;
		    case 2:
			// Whether the string entered by the user was binary.
			binary = false;
			while(!binary){
			    System.out.println("Please enter the string for which"
					       + " you want to check the Kolmogorov"
					       + " complexity.");
			    // String for checking Kolmogorov complexity.
			    String string = sc.next();
			    if(!Util.isBinaryString(string)){
				System.out.println("Wrong format! Please enter a"
						   + " binary string.");
				continue;
			    }
			    binary = true;
			    System.out.println("The Kolmogorov complexity for" 
					       + " string " + string + " is "
					       + u.getKolmogorov(string));
			}
			break;
		    case 3:
			// Whether the string entered by the user was binary.
			binary = false;
			while(!binary){
			    System.out.println("Please enter the string for which"
					       + " you want to obtain the largest"
					       + " program.");
			    // String for checking largest program.
			    String string = sc.next();
			    if(!Util.isBinaryString(string)){
				System.out.println("Wrong format! Please enter a"
						   + " binary string.");
				continue;
			    }
			    binary = true;
			    System.out.println("The largest program that generates"
					       + " the string " + string + " is"
					       + " program number "
					       + u.getLargestProgram(string) +
					       ".");
			}
			break;
		    case 4:
			System.out.println("The number of non-halting programs"
					   + " currently registered is: "
					   + u.getNonHaltingPrograms() + ".");
			break;
		    case 5:
			// Whether the string entered by the user was binary.
			binary = false;
			while(!binary){
			    System.out.println("Please enter the string for which"
					       + " you want to check the number of"
					       + " programs that generate it.");
			    // String for checking number of programs.
			    String string = sc.next();
			    if(!Util.isBinaryString(string)){
				System.out.println("Wrong format! Please enter a"
						   + " binary string.");
				continue;
			    }
			    binary = true;
			    System.out.println("The number of programs that"
					       + " generate string " + string
					       + " is "
					       + u.getNumberOfPrograms(string)
					       + ".");
			}
			break;
		    case 6:
			// Whether the string entered by the user was binary.
			binary = false;
			while(!binary){
			    System.out.println("Please enter the string for which"
					       + " you want to lookup the"
					       + " shortest program.");
			    // String for checking shortest program.
			    String string = sc.next();
			    if(!Util.isBinaryString(string)){
				System.out.println("Wrong format! Please enter a"
						   + " binary string.");
				continue;
			    }
			    binary = true;
			    System.out.println("The shortest program that"
					       + " generates string " + string
					       + " is "
					       + u.getShortestProgram(string)
					       + ".");
			}
			break;
		    case 7:
			// Whether the string entered by the user was binary.
			binary = false;
			while(!binary){
			    System.out.println("Please enter the string for which"
					       + " you want to lookup the"
					       + " universal distribution.");
			    // String for checking universal distribution.
			    String string = sc.next();
			    if(!Util.isBinaryString(string)){
				System.out.println("Wrong format! Please enter a"
						   + " binary string.");
				continue;
			    }
			    binary = true;
			    System.out.println("The universal distribution for"
					       + " string " + string + " is "
					       + u.getUniversalDist(string)
					       + ".");
			}
			break;
			case 8:
			    // Program to check
			    int program = -1;
			    while(program == -1){
				System.out.println("Which program would you like to"
						   + " search?");
				try{ 
				    program = sc.nextInt();
				}catch (InputMismatchException e){
				    System.out.println("Please enter a natural"
						       + " number.");
				    program = -1;
				    continue;
				}
			    }
			    if(u.inserted(program))
				System.out.println("The program: " + program
						   + " is currently in the"
						   + " database.");
			    else
				System.out.println("The program: " + program
						   + " is currently not in the"
						   + " database.");
			    break;
		    }
		} catch(SQLException e){
		    System.out.println(e.getMessage());
		    System.exit(1);
		}
		break;
	    case 3:
		System.out.println("Goodbye!");
		System.exit(0);
	    }
	}
    }
}
