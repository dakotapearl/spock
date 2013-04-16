package spock.dataaccess.ejb.interfaces.entities;

import java.io.Serializable;

/**
 *
 * @author loren
 */
public interface NetworkConnection extends Serializable {

    Long getId();
    NetworkNode getReceivingNode();
    NetworkNode getSendingNode();
    Double getStrength();
    void setId(Long id);
    void setReceivingNode(NetworkNode receivingNode);
    void setSendingNode(NetworkNode sendingNode);
    void setStrength(Double Strength);
    
}
