package spock.dataaccess.ejb.support;

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import java.security.*;
import spock.dataaccess.ejb.interfaces.BasicEntityCollection;
import spock.dataaccess.ejb.interfaces.UserFunctions;
import spock.dataaccess.ejb.interfaces.entities.User;
import spock.dataaccess.ejb.interfaces.entities.UserInterface;
import spock.dataaccess.ejb.support.collections.UserUserInterfaceCollection;
import spock.dataaccess.entities.UserEntity;

/**
 *
 * @author Loren Chorley
 */
public class ConcreteUserFunctions extends AbstractBasicEntity<User, String> implements UserFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.UserFunctions");
    
    private UserUserInterfaceCollection userUserInterfaceCollection;
    
    public ConcreteUserFunctions(EntityManager em) {
        super(em);
        userUserInterfaceCollection = new UserUserInterfaceCollection(em);
    }
    
    public BasicEntityCollection<User, UserInterface, Long> UserInterfaces() {
        return userUserInterfaceCollection;
    }
    
    public boolean verifiyUser(String username, String passwordHash) {
        
        try {
            
            boolean verified = false;
            
            // If there are no users, create a root user with a default password
            if (this.countEntities() == 0) {
                this.setEntity(new UserEntity("root", md5sum("admin"), "", UserFunctions.ACCESSRIGHTS_ADMIN));
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
            Logger.getLogger(ConcreteUserFunctions.class.getName()).log(Level.SEVERE, null, ex);
        } catch (NoSuchAlgorithmException ex) {
            Logger.getLogger(ConcreteUserFunctions.class.getName()).log(Level.SEVERE, null, ex);
        }
        
        return result;
        
        
    }

    @Override
    protected User newEntity(String id) {
        User u = new UserEntity();
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
        return "UserEntity";
    }

    @Override
    protected Class getEntityClass() {
        return UserEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(User entity) {
        
    }
    
}
