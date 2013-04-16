package spock.dataaccess.entities;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import spock.dataaccess.ejb.interfaces.entities.Network;
import spock.dataaccess.ejb.interfaces.entities.NetworkBehaviour;
import spock.dataaccess.ejb.interfaces.entities.NetworkInterface;
import spock.dataaccess.ejb.interfaces.entities.NetworkNode;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name = "NetworkNodes")
public class NetworkNodeEntity implements NetworkNode {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @NotNull
    @ManyToOne
    protected NetworkEntity network;
    @ManyToOne
    protected NetworkBehaviourEntity networkBehaviour;
    @ManyToOne
    protected NetworkInterfaceEntity networkInterface;

    public NetworkNodeEntity() {
    }

    public NetworkNodeEntity(Network network, NetworkBehaviour networkBehaviour, NetworkInterface networkInterface) {
        this.network = (NetworkEntity) network;
        this.networkBehaviour = (NetworkBehaviourEntity) networkBehaviour;
        this.networkInterface = (NetworkInterfaceEntity) networkInterface;
    }
    
    @Override
    public Long getId() {
        return id;
    }

    @Override
    public void setId(Long id) {
        this.id = id;
    }
    
    @Override
    public Network getNetwork() {
        return network;
    }

    @Override
    public void setNetwork(Network network) {
        this.network = (NetworkEntity) network;
    }

    @Override
    public NetworkBehaviour getNetworkBehaviour() {
        return networkBehaviour;
    }

    @Override
    public void setNetworkBehaviour(NetworkBehaviour networkBehaviour) {
        this.networkBehaviour = (NetworkBehaviourEntity) networkBehaviour;
    }
    
    @Override
    public NetworkInterface getNetworkInterface() {
        return networkInterface;
    }

    @Override
    public void setNetworkInterface(NetworkInterface networkInterface) {
        this.networkInterface = (NetworkInterfaceEntity) networkInterface;
    }
    
    
    @Override
    public int hashCode() {
        int hash = 0;
        hash += (id != null ? id.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof NetworkNodeEntity)) {
            return false;
        }
        NetworkNodeEntity other = (NetworkNodeEntity) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.NetworkNode[ id=" + id + " ]";
    }
    
}
