package spock.dataaccess.ejb.interfaces.entities;

import java.io.Serializable;

/**
 *
 * @author loren
 */
public interface NetworkNode extends Serializable {

    Long getId();
    Network getNetwork();
    NetworkBehaviour getNetworkBehaviour();
    NetworkInterface getNetworkInterface();
    void setId(Long id);
    void setNetwork(Network network);
    void setNetworkBehaviour(NetworkBehaviour networkBehaviour);
    void setNetworkInterface(NetworkInterface networkInterface);
    
}
