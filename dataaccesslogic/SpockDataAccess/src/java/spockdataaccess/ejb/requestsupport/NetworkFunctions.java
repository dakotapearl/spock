package spockdataaccess.ejb.requestsupport;

import spockdataaccess.ejb.requestsupport.generalisations.BasicEntity;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spockdataaccess.ejb.requestsupport.entitycollections.NetworkExperimentCollection;
import spockdataaccess.ejb.requestsupport.entitycollections.NetworkInterfaceCollection;
import spockdataaccess.ejb.requestsupport.entitycollections.NetworkMetricCollection;
import spockdataaccess.ejb.requestsupport.entitycollections.NetworkNodeCollection;
import spockdataaccess.entity.Network;
import spockdataaccess.entity.NetworkConnection;
import spockdataaccess.entity.NetworkNode;

/**
 * Encapsulates the functions necessary to manipulate the network table and its close associates
 * @author Loren Chorley
 */
public class NetworkFunctions extends BasicEntity<Network, String> {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.NetworkFunctions");
    
    public static final boolean INPUT_INTERFACE = true;
    public static final boolean OUTPUT_INTERFACE = false;
    
    private NetworkExperimentCollection networkExperimentCollection;
    private NetworkNodeCollection networkNodeCollection;
    private NetworkInterfaceCollection networkInterfaceCollection;
    private NetworkMetricCollection networkMetricCollection;
    
    public NetworkFunctions(EntityManager em) {
        super(em);
        this.em = em;
        networkExperimentCollection = new NetworkExperimentCollection(em);
        networkNodeCollection = new NetworkNodeCollection(em);
        networkInterfaceCollection = new NetworkInterfaceCollection(em);
        networkMetricCollection = new NetworkMetricCollection(em);
    }
    
    public NetworkExperimentCollection Experiments() {
        return networkExperimentCollection;
    }
    
    public NetworkNodeCollection Nodes() {
        return networkNodeCollection;
    }
    
    public NetworkInterfaceCollection Interfaces() {
        return networkInterfaceCollection;
    }
    
    public NetworkMetricCollection Metrics() {
        return networkMetricCollection;
    }
    
    public void connectNodes(Long sendingNodeId, Long receivingNodeId, Double strength) {
        try {
            
            NetworkNode sendingNode = em.find(NetworkNode.class, sendingNodeId);
            NetworkNode receivingNode = em.find(NetworkNode.class, receivingNodeId);
            
            // TODO change this to a remote exception
            if (sendingNode == null || receivingNode == null) {
                throw new EJBException("When connecting network nodes via node ID, at least one of the node IDs was not valid.");
            }
            
            connectNodes(sendingNode, receivingNode, strength);
            
        } catch (Exception ex) {
            throw new EJBException("RequestBean.createNetwork threw: " + ex.getMessage());
        }
        
    }
    
    /**
     * Connects two nodes in a network
     * @param sendingNodeId the ID of the node that will produce information
     * @param incomireceivingNodeIdngNetworkNodeId the ID of the node that will accept information
     */
    public void connectNodes(NetworkNode sendingNode, NetworkNode receivingNode, Double strength) {
        
        try {
        
            NetworkConnection networkConnection = new NetworkConnection();
            
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
    
    @Override
    protected Network newEntity(String id) {
        Network x = new Network();
        x.setId(id);
        return x;
    }

    @Override
    protected void copyEntityProperties(Network sourceEntity, Network targetEntity) {
        targetEntity.setIsActive(sourceEntity.getIsActive());
        targetEntity.getExperiments().clear();
        targetEntity.getExperiments().addAll(sourceEntity.getExperiments());
        targetEntity.getNetworkNodes().clear();
        targetEntity.getNetworkNodes().addAll(sourceEntity.getNetworkNodes());
        targetEntity.getNetworkInterfaces().clear();
        targetEntity.getNetworkInterfaces().addAll(sourceEntity.getNetworkInterfaces());
    }

    @Override
    protected String getEntityID(Network entity) {
        return entity.getId();
    }

    @Override
    protected String getEntityName() {
        return "Network";
    }

    @Override
    protected Class getEntityClass() {
        return Network.class;
    }

    @Override
    protected void verifyBusinessLogic(Network entity) {
        
    }
    
}
