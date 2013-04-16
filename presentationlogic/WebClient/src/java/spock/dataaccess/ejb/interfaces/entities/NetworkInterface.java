package spock.dataaccess.ejb.interfaces.entities;

import java.io.Serializable;
import java.util.Collection;

/**
 *
 * @author loren
 */
public interface NetworkInterface extends Serializable {

    void addNetworkNode(NetworkNode networkNode);
    Long getId();
    Boolean getIsInputInterface();
    Network getNetwork();
    Collection<NetworkNode> getNetworkNodes();
    Integer getNumberOfNodes();
    void setId(Long id);
    void setIsInputInterface(Boolean isInputInterface);
    void setNetwork(Network network);
    void setNetworkNodes(Collection<NetworkNode> networkNodes);
    void setNumberOfNodes(Integer NumberOfNodes);
    
}
