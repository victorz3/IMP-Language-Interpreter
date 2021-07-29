package mx.unam.universaldist;

import java.math.BigDecimal;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Class for reading/writing on the database and computing information about the
 * dataset.
 * @author Victor Zamora
 */
public class UniversalDistribution{

    // Connection to the database.
    private Conn connection;

    // Programs table name.
    private static final String progTable = "program";

    // Universal distribution table name.
    private static final String universalDistTable = "universal_dist";
    

    /**
     * Returns the connection to the database.
     * @return The connection to the database. 
     */
    public Conn getConnection(){
	if(connection == null)
	    connection = new Conn();
	if(!connection.isValid()){
	    connection.close();
	    connection = new Conn();
	}
	return connection;
    }

    /**
     * Returns the number of programs that halted.
     * @return Number of halting programs.
     * @throws SQLException if the query fails.
     */
    public int getHaltingPrograms() throws SQLException{
	Conn c = getConnection();
	String q = String.format("SELECT COUNT(*) AS num FROM %s WHERE "
				 + "result <> \"err\"", 
				 progTable);
	ResultSet r = c.query(q);
	int programs = 0;
	if(r.next())
	    programs = r.getInt(1);
	return programs;
    }
    
    /**
     * Returns the Kolmogorov complexity for a String s.
     * @param s - String for which the Kolmogorov complexity is looked up.
     * @return The Kolmogorov complexity of s or infinity if s hasn't been
     *         outputted yet.
     * @throws SQLException if the query fails.
     */
    public int getKolmogorov(String s) throws SQLException{
	Conn c = getConnection();
	String q = String.format("SELECT min(length) FROM %s WHERE result = \"%s\"",
				 progTable, s);
	ResultSet r = c.query(q);
	int min = Integer.MAX_VALUE;
	if(r.next())
	    min = r.getInt(1);
	return min;
    }

    /**
     * Returns the length of the largest program that outputs s.
     * @param s - A String.
     * @return The length of the largest program that outputs s.
     * @throws SQLException if the query fails.
     */
    public int getLargestProgram(String s) throws SQLException{
	Conn c = getConnection();
	String q = String.format("select number from %s where length = (select"
				 + " max(length) from %s where result = \"%s\")",
				 progTable, progTable, s);
	ResultSet r = c.query(q);
	int max = 0;
	if(r.next())
	    max = r.getInt(1);
	return max;
    }

    /**
     * Returns the number of programs that didn't halt.
     * @return The number of programs that didn't halt.
     * @throw SQLException if the query fails.
     */
    public int getNonHaltingPrograms() throws SQLException{
	Conn c = getConnection();
	String q = String.format("SELECT COUNT(*) AS num FROM %s WHERE "
				 + "result = \"err\"", 
				 progTable);
	ResultSet r = c.query(q);
	int programs = 0;
	if(r.next())
	    programs = r.getInt(1);
	return programs;
    }	

    /**
     * Returns the number of programs that have outputted s.
     * @param s - A String.
     * @return Number of programs that have outputted s.
     * @throws SQLException if the query fails.
     */
    public int getNumberOfPrograms(String s) throws SQLException{
 	Conn c = getConnection();
	String q = String.format("SELECT COUNT(*) AS num FROM %s WHERE result = \"%s\"", 
				 progTable, s);
	ResultSet r = c.query(q);
	int programs = 0;
	if(r.next())
	    programs = r.getInt(1);
	return programs;
    }
    
    /**
     * Returns the number of the shortest program that outputs s. 
     * @param s - A string
     * @return The shortest program that ouputs s.
     * @throws SQLException if the query fails.
     */
    public int getShortestProgram(String s) throws SQLException{
	Conn c = getConnection();
	String q = String.format("select number from %s where length = (select"
				 + " min(length) from %s where result = \"%s\")",
				 progTable, progTable, s);
	ResultSet r = c.query(q);
	int min = Integer.MAX_VALUE;
	if(r.next())
	    min = r.getInt(1);
	return min;
    }
   
    /**
     * Returns the current universal distribution value for String s.
     * @param s - String for which we will lookup the universal distribution.
     * @return The universal distribution value for s.
     * @throws SQLException if the lookup fails.
     */
    public double getUniversalDist(String s) throws SQLException{
	Conn c = getConnection();
	String q = String.format("SELECT value FROM %s WHERE string = \"%s\"",
				 universalDistTable, s);
	// Universal distribution value for the string.
	double value = 0;
	// Result set for the query.
	ResultSet r = c.query(q);
	if(r.next())
	    value = r.getDouble(1);
	return value;
    }

    
    /**
     * Returns true if the program has already been inserted into the database
     * and false otherwise.
     * @param result - The program's execution's result.
     * @return true if the program was already inserted and false otherwise.
     */
    public boolean inserted(String result){
	try{
	    Conn c = getConnection();
	    // Dividing the string by its parts.
	    String[] parts = result.split(" ");
	    // Query program
	    String q = String.format("SELECT * FROM %s WHERE number = %d",
				     progTable, Integer.parseInt(parts[0]));
	    // Result set for the query.
	    ResultSet r = c.query(q);
	    return r.isBeforeFirst();
	}catch(SQLException e){
	    System.err.println(e.getMessage());
	}
	return true;
    }

    
    /* Returns the length of the number n in a base b, prefix-free
     * enumeration. This method doesn't work for powers of 2.
     */
    public int numLength(int n, int b){
	// Base 2 logarithm of b.
        double base2Log = Math.log(b) / Math.log(2);
	// Digits to be used per character.
	int digits = (int) Math.ceil(base2Log);
	if(n == 0)
	    return 2*digits;
	int baseBLog = (int) (Math.log(n) / Math.log(b));
	System.out.println("¿Cuánto vale este logarimo wtf?: " + baseBLog);
	return (baseBLog + 2) * digits;
    }
    
    
    /**
     * Writes the result of an execution in the database. 
     * @param result - Result of an execution in the format "program# result 
     * len(program) steps".
     * @return true if the write was successful and false otherwise.
     */ 
    public boolean writeResult(String result){
	try{
	    // If program has been inserted, return false.
	    if (inserted(result))
		return false;
	    // Dividing the string by its parts.
	    String[] parts = result.split(" ");
	    int length = numLength(Integer.parseInt(parts[0]), 15);
	    String u = String.format("INSERT INTO %s VALUES(%s, \"%s\", %d, %s)",
				     progTable, parts[0], parts[1], length,
				     parts[3]);
      	    Conn c = getConnection();
	    c.update(u);
	    // Now update universal distribution.
	    if(parts[1].equals("err"))
		return false;
	    System.out.println("Program: " + parts[0]);
	    BigDecimal value = new BigDecimal(getUniversalDist(parts[1]));
	    BigDecimal power = (new BigDecimal(2)).pow(length);
	    
	    value = value.add(BigDecimal.ONE.divide(power));
	    	    
	    u = String.format("REPLACE INTO %s VALUES(\"%s\", %s)",
			      universalDistTable, parts[1], value.toString());
	    c.update(u);
	}catch(SQLException e){
	    System.err.println(e.getMessage());
	    return false;
	}
	return true;
    }
}
