package spockdataaccess.ejb.requestsupport;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spockdataaccess.entity.Network;
import spockdataaccess.entity.NetworkConnection;
import spockdataaccess.entity.NetworkInterface;
import spockdataaccess.entity.NetworkNode;

/**
 * Encapsulates the functions necessary to manipulate the network table and its close associates
 * @author Loren Chorley
 */
public class NetworkFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.NetworkFunctions");
    
    public static final boolean INPUT_INTERFACE = true;
    public static final boolean OUTPUT_INTERFACE = false;
    
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
     * @param sendingNodeId the ID of the node that will produce information
     * @param incomireceivingNodeIdngNetworkNodeId the ID of the node that will accept information
     */
    public void connectNodes(Long sendingNodeId, Long receivingNodeId, Double strength) {
        
        try {
        
            NetworkConnection networkConnection = new NetworkConnection();
            
            NetworkNode sendingNode = em.find(NetworkNode.class, sendingNodeId);
            NetworkNode receivingNode = em.find(NetworkNode.class, receivingNodeId);
            
            // Add not null verifications
            
            networkConnection.setSendingNode(sendingNode);
            networkConnection.setReceivingNode(receivingNode);
            networkConnection.setStrength(strength);
            
            em.persist(networkConnection);
            
            logger.log(Level.INFO,
                       "Established connection between nodes: {0} -> {1} with strength {2}",
                       new Object[] { sendingNode.getId(), receivingNode.getId(), strength });
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.createNetwork threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Creates an network interface, the right amount of network nodes and connects them all appropriately
     * @param networkId the network for the network interface
     * @param interfaceIsInput If this is true then the interface is for input, otherwise it is for output
     * @param numberOfNodes determines the number of nodes that are attached to this interface
     * @return returns the interface ID
     */
    public Long createNetworkInterface(
            String networkId,
            boolean interfaceType,
            Integer numberOfNodes) {
        
        try {
            
            Network network = em.find(Network.class, networkId);
            
            // Create interface
            NetworkInterface networkInterface = new NetworkInterface();
            
            networkInterface.setIsInputInterface(interfaceType);
            networkInterface.setNumberOfNodes(numberOfNodes);

            // Add interface to network, and vise versa
            network.addNetworkInterface(networkInterface);
            networkInterface.setNetwork(network);
            
            em.persist(networkInterface);
            
            // Create exactly the right number of network nodes and add them to both the interface and the enivornemnt
            NetworkNode networkNode;
            for (int i = 0; i<numberOfNodes; i++) {
                networkNode = new NetworkNode();
                
                network.addNetworkNode(networkNode);
                networkNode.setNetwork(network);
                
                networkInterface.addNetworkNode(networkNode);
                networkNode.setNetworkInterface(networkInterface);
                
                em.persist(networkNode);
                
            }
            
            
            logger.log(Level.INFO,
                       "Created and persisted network interface: {0}",
                       new Object[] { networkInterface.getId() });
            
            return networkInterface.getId();
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.createNetwork threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * 
     * @param networkInterfaceId 
     */
    public void removeNetworkInterface(Long networkInterfaceId) {
        try {
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.createNetwork threw: " + ex.getMessage());
        }
    }
    
}
