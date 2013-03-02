package spockdataaccess.entity;

import java.io.Serializable;
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

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name="NetworkInterfaces")
public class NetworkInterface implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    protected Integer NumberOfNodes;
    @OneToMany(mappedBy="networkInterface")
    protected Collection<NetworkNode> networkNodes;
    @NotNull
    @ManyToOne
    protected Network network;
    @NotNull
    protected Boolean isInputInterface;
    
    public NetworkInterface() {
        networkNodes = new ArrayList<NetworkNode>();
    }
    
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Integer getNumberOfNodes() {
        return NumberOfNodes;
    }

    public void setNumberOfNodes(Integer NumberOfNodes) {
        this.NumberOfNodes = NumberOfNodes;
    }

    public Collection<NetworkNode> getNetworkNodes() {
        return networkNodes;
    }

    public void setNetworkNodes(Collection<NetworkNode> networkNodes) {
        this.networkNodes = networkNodes;
    }

    public void addNetworkNode(NetworkNode networkNode) {
        networkNodes.add(networkNode);
    }

    public Boolean getIsInputInterface() {
        return isInputInterface;
    }

    public void setIsInputInterface(Boolean isInputInterface) {
        this.isInputInterface = isInputInterface;
    }

    public Network getNetwork() {
        return network;
    }

    public void setNetwork(Network network) {
        this.network = network;
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
        if (!(object instanceof NetworkInterface)) {
            return false;
        }
        NetworkInterface other = (NetworkInterface) object;
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
