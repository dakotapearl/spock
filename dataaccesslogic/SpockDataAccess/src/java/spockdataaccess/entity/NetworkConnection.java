package spockdataaccess.entity;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name="NetworkConnections")
public class NetworkConnection implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @NotNull
    protected Double Strength;
    @NotNull
    @ManyToOne
    protected NetworkNode sendingNode;
    @NotNull
    @ManyToOne
    protected NetworkNode receivingNode;

    public NetworkConnection() {
    }

    public NetworkConnection(Double Strength, NetworkNode sendingNode, NetworkNode receivingNode) {
        this.Strength = Strength;
        this.sendingNode = sendingNode;
        this.receivingNode = receivingNode;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Double getStrength() {
        return Strength;
    }

    public void setStrength(Double Strength) {
        this.Strength = Strength;
    }

    public NetworkNode getReceivingNode() {
        return receivingNode;
    }

    public void setReceivingNode(NetworkNode receivingNode) {
        this.receivingNode = receivingNode;
    }

    public NetworkNode getSendingNode() {
        return sendingNode;
    }

    public void setSendingNode(NetworkNode sendingNode) {
        this.sendingNode = sendingNode;
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
        if (!(object instanceof NetworkConnection)) {
            return false;
        }
        NetworkConnection other = (NetworkConnection) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.NetworkConnection[ id=" + id + " ]";
    }
    
}
