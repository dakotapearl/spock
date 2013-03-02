package spockdataaccess.ejb.requestsupport;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spockdataaccess.entity.NetworkBehaviour;

/**
 * Allows access to the NetworkBehaviour entity and it's database
 * @author Loren Chorley
 */
public class BehaviourFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.BehaviourFunctions");
    
    private EntityManager em;
    
    public BehaviourFunctions(EntityManager em) {
        this.em = em;
    }
    
    /**
     * Set a value in or create a new network behaviour
     * @param name the ID of the network behaviour
     * @param codeURL the location of the binary classes for the network behaviour
     */
    public void setBehaviour(String name, String codeURL) {
        
        try {
            
            NetworkBehaviour networkBehaviour = em.find(NetworkBehaviour.class, name);
            
            if (networkBehaviour == null) {
                
                networkBehaviour = new NetworkBehaviour();
                
                logger.log(Level.INFO,
                           "Created new network behaviour: {0}",
                           new Object[] { name });
                
            }
            
            networkBehaviour.setId(name);
            networkBehaviour.setCodeURL(codeURL);
            
            em.persist(networkBehaviour);
            
            logger.log(Level.INFO,
                       "Set network behaviour: {0}-{1}",
                       new Object[] { name, codeURL });
            
        } catch (Exception ex) {
            throw new EJBException("BehaviourFunctions.addBehaviour threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Retrieve the Code RUL for a network behaviour
     * @param name the ID of the network behaviour
     * @return the code URL
     */
    public String getBehaviourCodeURL(String name) {
        String rtnval = null;
        
        try {
            
            NetworkBehaviour networkBehaviour = em.find(NetworkBehaviour.class, name);
            rtnval = networkBehaviour.getCodeURL();
            
        } catch (Exception ex) {
            throw new EJBException("BehaviourFunctions.getBehaviour threw: " + ex.getMessage());
        } finally {
            return rtnval;
        }
        
    }
    
    /**
     * Remove the network behaviour from the database
     * @param name the ID of the network behaviour
     */
    public void removeBehaviour(String name) {
        try {
            
            NetworkBehaviour networkBehaviour = em.find(NetworkBehaviour.class, name);
            em.remove(networkBehaviour);
            
            logger.log(Level.INFO,
                       "Removed network behaviour: {0}",
                       new Object[] { name });
            
        } catch (Exception ex) {
            throw new EJBException("BehaviourFunctions.removeBehaviour threw: " + ex.getMessage());
        }
    }
    
}
