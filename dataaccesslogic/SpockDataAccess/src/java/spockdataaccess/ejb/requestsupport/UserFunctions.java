package spockdataaccess.ejb.requestsupport;

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spockdataaccess.entity.User;
import java.security.*;
import java.util.Arrays;

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
            
            User user = new User();
            user.setUsername(Username);
            user.setPassword(md5sum(Password));
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
     * Sets the password of the user, encrypted via md5
     * @param Username the username of the user
     * @param Password the new password of the user
     */
    public void setPassword(String Username, String Password) {
        try {
        
            User user = em.find(User.class, Username);
            user.setPassword(md5sum(Password));
        
            logger.log(Level.INFO,
                       "Set password of user {0} to {1}",
                       new Object[] { user.getUsername() , user.getPassword() });
            
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
    
    /**
     * Counts the number of users registered in the database
     * @return the number of users in the database
     */
    public Integer countUsers() {
        
        try {
            
            Integer count = em.createNamedQuery("countUsers").getFirstResult();
            
            logger.log(Level.INFO,
                       "User count: {0}",
                       new Object[] { count });
            
            return count;
            
        } catch (Exception ex) {
            throw new EJBException("UserFunctions.removeUser threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Encrypts the given string via md5
     * @param str string to be converted
     * @return returns a hex string
     */
    public String md5sum(String str) {
        String result = "";
        
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            byte[] bytesOfMessage = str.getBytes("UTF-8");
            byte[] encryptedPassword = md.digest(bytesOfMessage);
            
            result = new BigInteger(1, encryptedPassword).toString(16);
            
        } catch (UnsupportedEncodingException ex) {
            Logger.getLogger(UserFunctions.class.getName()).log(Level.SEVERE, null, ex);
        } catch (NoSuchAlgorithmException ex) {
            Logger.getLogger(UserFunctions.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            return result;
        }
        
    }
    
}
