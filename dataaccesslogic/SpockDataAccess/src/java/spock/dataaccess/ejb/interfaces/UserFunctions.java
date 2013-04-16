package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;
import spock.dataaccess.ejb.interfaces.entities.User;
import spock.dataaccess.ejb.interfaces.entities.UserInterface;

/**
 *
 * @author Loren Chorley
 */
public interface UserFunctions extends Serializable, BasicEntity<User, String> {
    
    public static final String ACCESSRIGHTS_ADMIN = "admin";
    public static final String ACCESSRIGHTS_USER = "user";    
    
    public BasicEntityCollection<User, UserInterface, Long> UserInterfaces();
    
    /**
     * 
     * @param username
     * @param passwordHash
     * @return 
     */
    public boolean verifiyUser(String username, String passwordHash);
    
    /**
     * Encrypts the given string via md5
     * @param str string to be converted
     * @return returns a hex string
     */
    public String md5sum(String str);
    
}
