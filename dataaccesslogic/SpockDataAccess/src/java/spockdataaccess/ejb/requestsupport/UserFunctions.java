package spockdataaccess.ejb.requestsupport;

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
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
public class UserFunctions extends BasicEntityFunctions<User, String> {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.UserFunctions");
    
    public static final String ACCESSRIGHTS_ADMIN = "admin";
    public static final String ACCESSRIGHTS_USER = "user";
    
    private EntityManager em;
    
    public UserFunctions(EntityManager em) {
        super(em);
    }
       
    public boolean verifiyUser(String username, String passwordHash) {
        
        try {
            
            boolean verified = false;
            
            // If there are no users, create a root user with a default password
            if (this.countEntities() == 0) {
                this.setEntity(new User("root", md5sum("admin"), "", ACCESSRIGHTS_ADMIN));
            }
            
            // Check to see if there is a user that matches
            User user = (User) this.retrieveEntity(username).get(0);
            
            if (user != null) {
                if (user.getUsername().equals(username) && user.getPassword().equals(passwordHash)) {
                    verified = true;
                }
            }
            
            return verified;
        
        } catch (Exception ex) {
            throw new EJBException("UserFunctions.verifiyUser threw: " + ex.getMessage());
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
        }
        
        return result;
        
        
    }

    @Override
    protected User newEntity(String id) {
        User u = new User();
        u.setUsername(id);
        return u;
    }

    @Override
    protected void copyEntityProperties(User sourceEntity, User targetEntity) {
        targetEntity.setPassword(sourceEntity.getPassword());
        targetEntity.setEmail(sourceEntity.getEmail());
        targetEntity.setAccessRights(sourceEntity.getAccessRights());
        targetEntity.getInterfaces().clear();
        targetEntity.getInterfaces().addAll(sourceEntity.getInterfaces());
    }

    @Override
    protected String getEntityID(User entity) {
        return entity.getUsername();
    }

    @Override
    protected String getEntityName() {
        return "User";
    }

    @Override
    protected Class getEntityClass() {
        return User.class;
    }
    
}
