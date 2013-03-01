package spockdataaccess.entity;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name = "NetworkNodes")
public class NetworkNode implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @NotNull
    @ManyToOne
    protected Network network;
    @ManyToMany
    protected Collection<NetworkNode> outgoingNodes;
    @ManyToMany(mappedBy="outgoingNodes")
    protected Collection<NetworkNode> incomingNodes;

    public NetworkNode() {
        outgoingNodes = new ArrayList<NetworkNode>();
        incomingNodes = new ArrayList<NetworkNode>();
    }
    
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }
    
    public Network getNetwork() {
        return network;
    }

    public void setNetwork(Network network) {
        this.network = network;
    }
    
    public Collection<NetworkNode> getIncomingNodes() {
        return incomingNodes;
    }

    public Collection<NetworkNode> getOutgoingNodes() {
        return outgoingNodes;
    }

    public void addOutgoingNode(NetworkNode node) {
        outgoingNodes.add(node);
    }

    public void addIncomingNode(NetworkNode node) {
        incomingNodes.add(node);
    }
    
    public void removeOutgoingNode(NetworkNode node) {
        outgoingNodes.remove(node);
    }

    public void removeIncomingNode(NetworkNode node) {
        incomingNodes.remove(node);
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
        if (!(object instanceof NetworkNode)) {
            return false;
        }
        NetworkNode other = (NetworkNode) object;
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
