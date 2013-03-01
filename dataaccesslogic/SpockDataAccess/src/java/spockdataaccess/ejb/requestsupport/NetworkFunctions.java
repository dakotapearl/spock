package spockdataaccess.ejb.requestsupport;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spockdataaccess.entity.Network;
import spockdataaccess.entity.NetworkNode;

/**
 * Encapsulates the functions necessary to manipulate the network table and its close associates
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
    
    /**
     * Adds a new network node to a network
     * @param networkid the ID for the network to which to add a node
     * @return returns the ID of the new network node
     */
    public Long addNodeToNetwork(String networkid) {
        
        try {
        
            Network network = em.find(Network.class, networkid);
            NetworkNode node = new NetworkNode();
            
            node.setNetwork(network);
            
            em.persist(node);
            
            logger.log(Level.INFO,
                       "Created and persisted network node: {0}",
                       new Object[] { node.getId() });
            
            return node.getId();
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.createNetwork threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Connects two nodes in a network
     * @param outgoingNetworkNodeId the ID of the node that will produce information
     * @param incomingNetworkNodeId the ID of the node that will accept information
     */
    public void connectNodes(Long outgoingNetworkNodeId, Long incomingNetworkNodeId) {
        
        try {
        
            NetworkNode nodeOutgoing = em.find(NetworkNode.class, outgoingNetworkNodeId);
            NetworkNode nodeIncoming = em.find(NetworkNode.class, incomingNetworkNodeId);
            
            nodeOutgoing.addOutgoingNode(nodeIncoming);
            nodeIncoming.addIncomingNode(nodeOutgoing);
            
            logger.log(Level.INFO,
                       "Established connection between nodes: {0} -> {1}",
                       new Object[] { nodeOutgoing.getId(), nodeIncoming.getId() });
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.createNetwork threw: " + ex.getMessage());
        }
        
    }
    
}
