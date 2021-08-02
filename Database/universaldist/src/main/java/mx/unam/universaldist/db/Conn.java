package mx.unam.universaldist.db;

/**
 * Class for handling connections to the database. 
 * @author Victor Zamora
 */

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

public class Conn{

    private static final String db_name = "distribution.db";
    
    private Connection c = null; /* Connection to the database. */
    private Statement stmt = null; 
	
    
    /**
     * Creates a connection to our database. 
     */
    public Conn(){
	// Try to connect to DB... 
	try{
	    Class.forName("org.sqlite.JDBC");
	    // Initialize connection.
	    c = DriverManager.getConnection("jdbc:sqlite:db/" + db_name); 
	    c.setAutoCommit(false);
	    stmt = c.createStatement();
	}catch (Exception e) {
		System.err.println(e.getClass().getName() +
				   ": " + e.getMessage());
		System.exit(-1);
	}
    }
    
    /**
     * Method for executing queries.
     * @param query - Query to execute.
     * @return The queries ResultSet
     */
    public ResultSet query(String query){
	ResultSet rs = null;
	try{
	    rs = stmt.executeQuery(query);
	}catch(SQLException e){
	    System.err.println(e.getMessage());
	}
	return rs;
    }

    /**
     * Updates or insert a row in our database.
     * @param update - Update to execute.
     */
    public void update(String update){
	try{
	    stmt.executeUpdate(update);
	}catch(SQLException e){
	    System.err.println(e.getMessage());
	}
    }

    /**
     * Returns true if the connection is valid and false otherwise. 
     * @return true if the connection is valid, false otherwise */
    public boolean isValid(){
	try{
	    return this.c.isValid(0);
	}catch(SQLException e){
	    return false;
	}
    }

    /**
     * Closes the connection.
     */
    public void close(){
	try { if (stmt != null) stmt.close(); } catch (Exception e) {};
	try { if (c != null){c.commit(); c.close();} } catch (Exception e) {};
    }
}
