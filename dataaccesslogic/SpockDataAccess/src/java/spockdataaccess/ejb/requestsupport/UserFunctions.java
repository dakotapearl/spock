package spockdataaccess.ejb.requestsupport;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spockdataaccess.entity.User;
import java.security.*;

/**
 *
 * @author Loren Chorley
 */
public class UserFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.UserFunctions");
    
    private EntityManager em;
    
    public UserFunctions(EntityManager em) {
        this.em = em;
    }
    
    /**
     * Creates a new user record in the database.
     * @param Username the username of the new user
     * @param Password the unencrypted password of the new user
     * @param Email the email address of the user
     */
    public void createUser(String Username, String Password, String Email) {
        
        try {
        
            // Encrypt password
            MessageDigest md = MessageDigest.getInstance("MD5");
            byte[] bytesOfMessage = Password.getBytes("UTF-8");
            byte[] encryptedPassword = md.digest(bytesOfMessage);
            
            User user = new User();
            user.setUsername(Username);
            user.setPassword(encryptedPassword.toString());
            user.setEmail(Email);
            
            em.persist(user);
            
            logger.log(Level.INFO,
                       "Created and persisted user {0} with encrypted password {1}",
                       new Object[] { user.getUsername() , user.getPassword() });
            
        } catch (Exception ex) {
            throw new EJBException("UserFunctions.createUser threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * 
     * @param OldUsername
     * @param NewUsername 
     */
    public void setUsername(String OldUsername, String NewUsername) {
        try {
        
            
        
        } catch (Exception ex) {
            throw new EJBException("UserFunctions.setUsername threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * 
     * @param Username
     * @param Password 
     */
    public void setPassword(String Username, String Password) {
        try {
        
            
        
        } catch (Exception ex) {
            throw new EJBException("UserFunctions.setPassword threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * 
     * @param Username
     * @param Email 
     */
    public void setEmail(String Username, String Email) {
        try {
        
            
        
        } catch (Exception ex) {
            throw new EJBException("UserFunctions.setEmail threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Creates a new network record in the database.
     * @param id the ID of the network
     */
    public void removeUser(String Username) {
        
        try {
        
            User user = em.find(User.class, Username);
            em.remove(user);
            
            logger.log(Level.INFO,
                       "Removed user: {0}",
                       new Object[] { Username });
            
        } catch (Exception ex) {
            throw new EJBException("UserFunctions.removeUser threw: " + ex.getMessage());
        }
        
    }
}
