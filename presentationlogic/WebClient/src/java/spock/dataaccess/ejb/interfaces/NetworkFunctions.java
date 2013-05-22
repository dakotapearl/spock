package spock.dataaccess.ejb.interfaces;

import java.io.Serializable;
import javax.ejb.Remote;
import spock.dataaccess.ejb.interfaces.entities.Experiment;
import spock.dataaccess.ejb.interfaces.entities.Metric;
import spock.dataaccess.ejb.interfaces.entities.Network;
import spock.dataaccess.ejb.interfaces.entities.NetworkInterface;
import spock.dataaccess.ejb.interfaces.entities.NetworkNode;

/**
 * Encapsulates the functions necessary to manipulate the network table and its close associates
 * @author Loren Chorley
 */
@Remote
public interface NetworkFunctions extends Serializable, BasicEntity<Network, String> {
    
    public static final boolean INPUT_INTERFACE = true;
    public static final boolean OUTPUT_INTERFACE = false;
    
    public BasicEntityCollection<Network, Experiment, String> Experiments();
    public BasicEntityCollection<Network, NetworkNode, Long> Nodes();
    public BasicEntityCollection<Network, NetworkInterface, Long> Interfaces();
    public BasicEntityCollection<Network, Metric, String> Metrics();
    
    /**
     * 
     * @param sendingNodeId
     * @param receivingNodeId
     * @param strength 
     */
    public void connectNodes(Long sendingNodeId, Long receivingNodeId, Double strength);
    
    /**
     * Connects two nodes in a network
     * @param sendingNodeId the ID of the node that will produce information
     * @param incomireceivingNodeIdngNetworkNodeId the ID of the node that will accept information
     */
    public void connectNodes(NetworkNode sendingNode, NetworkNode receivingNode, Double strength);
    
}
