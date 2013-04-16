package spock.dataaccess.ejb.support;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.ejb.EJBException;
import javax.persistence.EntityManager;
import spock.dataaccess.ejb.interfaces.BasicEntityCollection;
import spock.dataaccess.ejb.interfaces.NetworkFunctions;
import spock.dataaccess.ejb.interfaces.entities.Experiment;
import spock.dataaccess.ejb.interfaces.entities.Metric;
import spock.dataaccess.ejb.interfaces.entities.Network;
import spock.dataaccess.ejb.interfaces.entities.NetworkInterface;
import spock.dataaccess.ejb.interfaces.entities.NetworkNode;
import spock.dataaccess.ejb.support.collections.NetworkExperimentCollection;
import spock.dataaccess.ejb.support.collections.NetworkInterfaceCollection;
import spock.dataaccess.ejb.support.collections.NetworkMetricCollection;
import spock.dataaccess.ejb.support.collections.NetworkNodeCollection;
import spock.dataaccess.entities.NetworkConnectionEntity;
import spock.dataaccess.entities.NetworkEntity;
import spock.dataaccess.entities.NetworkNodeEntity;

/**
 * Encapsulates the functions necessary to manipulate the network table and its close associates
 * @author Loren Chorley
 */
public class ConcreteNetworkFunctions extends AbstractBasicEntity<Network, String> implements NetworkFunctions {
    private static final Logger logger = Logger.getLogger("spockdataaccess.ejb.requestsupport.NetworkFunctions");
    
    private NetworkExperimentCollection networkExperimentCollection;
    private NetworkNodeCollection networkNodeCollection;
    private NetworkInterfaceCollection networkInterfaceCollection;
    private NetworkMetricCollection networkMetricCollection;
    
    public ConcreteNetworkFunctions(EntityManager em) {
        super(em);
        this.em = em;
        networkExperimentCollection = new NetworkExperimentCollection(em);
        networkNodeCollection = new NetworkNodeCollection(em);
        networkInterfaceCollection = new NetworkInterfaceCollection(em);
        networkMetricCollection = new NetworkMetricCollection(em);
    }
    
    public BasicEntityCollection<Network, Experiment, String> Experiments() {
        return networkExperimentCollection;
    }
    
    public BasicEntityCollection<Network, NetworkNode, Long> Nodes() {
        return networkNodeCollection;
    }
    
    public BasicEntityCollection<Network, NetworkInterface, Long> Interfaces() {
        return networkInterfaceCollection;
    }
    
    public BasicEntityCollection<Network, Metric, String> Metrics() {
        return networkMetricCollection;
    }
    
    public void connectNodes(Long sendingNodeId, Long receivingNodeId, Double strength) {
        try {
            
            NetworkNodeEntity sendingNode = em.find(NetworkNodeEntity.class, sendingNodeId);
            NetworkNodeEntity receivingNode = em.find(NetworkNodeEntity.class, receivingNodeId);
            
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
    @Override
    public void connectNodes(NetworkNode sendingNode, NetworkNode receivingNode, Double strength) {
        
        try {
        
            NetworkConnectionEntity networkConnection = new NetworkConnectionEntity();
            
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
        Network x = new NetworkEntity();
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
        return "NetworkEntity";
    }

    @Override
    protected Class getEntityClass() {
        return NetworkEntity.class;
    }

    @Override
    protected void verifyBusinessLogic(Network entity) {
        
    }
    
}
