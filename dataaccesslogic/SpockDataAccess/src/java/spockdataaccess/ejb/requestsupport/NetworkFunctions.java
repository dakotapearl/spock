/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package spockdataaccess.ejb.requestsupport;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spockdataaccess.entity.Network;

/**
 *
 * @author Loren Chorley
 */
public class NetworkFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.NetworkFunctions");
    
    private EntityManager em;
    
    public NetworkFunctions(EntityManager em) {
        this.em = em;
    }
    
    /**
     * Creates a new network record in the database.
     * @param id the ID of the network
     */
    public void createNetwork(String id) {
        
        try {
        
            Network network = new Network();
            network.setId(id);
            network.setIsActive(Boolean.FALSE);

            em.persist(network);
            
            logger.log(Level.INFO,
                       "Created and persisted network: {0}",
                       new Object[] { id });
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.createNetwork threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Sets the activation status of an network. True indicates that the network is currently active or running and may be stopped, and false that it is available to be started.
     * @param id the ID of the network
     * @param activation the new activation status of the network
     */
    public void setNetworkActivation(String id, boolean activation) {
        try {
        
            Network network = em.find(Network.class, id);
            
            if (network != null) {
                network.setIsActive(activation);
            } else {
                throw new EJBException("Network '" + id + "' not found when attempting to set activation property");
            }
        
        } catch (Exception ex) {
            throw new EJBException("RequestBean.setNetworkActivation threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Creates a new network record in the database.
     * @param id the ID of the network
     */
    public void removeNetwork(String id) {
        
        try {
        
            Network network = em.find(Network.class, id);
            em.remove(network);
            
            logger.log(Level.INFO,
                       "Removed network: {0}",
                       new Object[] { id });
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.removeNetwork threw: " + ex.getMessage());
        }
        
    }
    
    
}
