package spock.dataaccess.entities;

import java.util.ArrayList;
import java.util.Collection;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import spock.dataaccess.ejb.interfaces.entities.Network;
import spock.dataaccess.ejb.interfaces.entities.NetworkInterface;
import spock.dataaccess.ejb.interfaces.entities.NetworkNode;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name="NetworkInterfaces")
public class NetworkInterfaceEntity implements NetworkInterface {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    protected Integer NumberOfNodes;
    @OneToMany(mappedBy="networkInterface")
    protected Collection<NetworkNodeEntity> networkNodes;
    @NotNull
    @ManyToOne
    protected NetworkEntity network;
    @NotNull
    protected Boolean isInputInterface;
    
    public NetworkInterfaceEntity() {
        networkNodes = new ArrayList<NetworkNodeEntity>();
    }

    public NetworkInterfaceEntity(Integer NumberOfNodes, Network network, Boolean isInputInterface) {
        this.NumberOfNodes = NumberOfNodes;
        this.network = (NetworkEntity) network;
        this.isInputInterface = isInputInterface;
        networkNodes = new ArrayList<NetworkNodeEntity>();
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
    public Integer getNumberOfNodes() {
        return NumberOfNodes;
    }

    @Override
    public void setNumberOfNodes(Integer NumberOfNodes) {
        this.NumberOfNodes = NumberOfNodes;
    }

    @Override
    public Collection<NetworkNode> getNetworkNodes() {
        return (Collection) networkNodes;
    }

    @Override
    public void setNetworkNodes(Collection<NetworkNode> networkNodes) {
        this.networkNodes = (Collection) networkNodes;
    }

    @Override
    public void addNetworkNode(NetworkNode networkNode) {
        networkNodes.add((NetworkNodeEntity) networkNode);
    }

    @Override
    public Boolean getIsInputInterface() {
        return isInputInterface;
    }

    @Override
    public void setIsInputInterface(Boolean isInputInterface) {
        this.isInputInterface = isInputInterface;
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
    public int hashCode() {
        int hash = 0;
        hash += (id != null ? id.hashCode() : 0);
        return hash;
    }

    @Override
    public boolean equals(Object object) {
        // TODO: Warning - this method won't work in the case the id fields are not set
        if (!(object instanceof NetworkInterfaceEntity)) {
            return false;
        }
        NetworkInterfaceEntity other = (NetworkInterfaceEntity) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.NetworkInterface[ id=" + id + " ]";
    }
    
}
