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
import java.util.List;

/**
 *
 * @author Loren Chorley
 */
public class UserFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.UserFunctions");
    
    public static final String ACCESSRIGHTS_ADMIN = "admin";
    public static final String ACCESSRIGHTS_USER = "user";
    
    private EntityManager em;
    
    public UserFunctions(EntityManager em) {
        this.em = em;
    }
    
    /**
     * Creates a new user record in the database.
     * @param username the username of the new user
     * @param password the unencrypted password of the new user
     * @param email the email address of the user
     * @param accessRights the access rights of the user
     */
    public void createUser(String username, String passwordHash, String email, String accessRights) {
        
        try {
            
            User user = new User();
            user.setUsername(username);
            user.setPassword(passwordHash);
            user.setEmail(email);
            user.setAccessRights(accessRights);
            
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
    public void setUsername(String oldUsername, String newUsername) {
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
    public void setPassword(String username, String password) {
        try {
        
            User user = em.find(User.class, username);
            user.setPassword(md5sum(password));
        
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
    public void setEmail(String username, String email) {
        try {
        
            
        
        } catch (Exception ex) {
            throw new EJBException("UserFunctions.setEmail threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Creates a new network record in the database.
     * @param id the ID of the network
     */
    public void removeUser(String username) {
        
        try {
        
            User user = em.find(User.class, username);
            em.remove(user);
            
            logger.log(Level.INFO,
                       "Removed user: {0}",
                       new Object[] { username });
            
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
    
    public List<User> getAllUsers() {
        List<User> users = (List<User>) em.createQuery("SELECT u FROM User u").getResultList();

        return users;
    }
    
    public boolean verifiyUser(String username, String passwordHash) {
        boolean verified = false;
        
        try {
            
            // If there are no users, create a root user with a default password
            Integer userCount = countUsers();

            if (userCount == 0) {
                createUser("root", md5sum("admin"), "", "admin");
            }
            
            // Check to see if there is a user that matches
            Integer resultCount = em.createNamedQuery("verifyUser")
                                    .setParameter("uname", username)
                                    .setParameter("pword", passwordHash)
                                    .getResultList()
                                    .size();
            
            logger.log(Level.INFO,
                       "User verification returned {0} result for user {1}",
                       new Object[] { resultCount, username });
            
            if (resultCount == 1) {
                verified = true;
            } else if (resultCount != 0) {
                throw new EJBException("verifyUser threw unexpected result, non-0, non-1 number");
            }
            
        } catch (Exception ex) {
            throw new EJBException("UserFunctions.removeUser threw: " + ex.getMessage());
        } finally {
            return verified;
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
