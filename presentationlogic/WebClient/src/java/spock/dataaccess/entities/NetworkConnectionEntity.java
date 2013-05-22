package spock.dataaccess.entities;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import spock.dataaccess.ejb.interfaces.entities.NetworkConnection;
import spock.dataaccess.ejb.interfaces.entities.NetworkNode;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name="NetworkConnections")
public class NetworkConnectionEntity implements NetworkConnection {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @NotNull
    protected Double Strength;
    @NotNull
    @ManyToOne
    protected NetworkNodeEntity sendingNode;
    @NotNull
    @ManyToOne
    protected NetworkNodeEntity receivingNode;

    public NetworkConnectionEntity() {
    }

    public NetworkConnectionEntity(Double Strength, NetworkNode sendingNode, NetworkNode receivingNode) {
        this.Strength = Strength;
        this.sendingNode = (NetworkNodeEntity) sendingNode;
        this.receivingNode = (NetworkNodeEntity) receivingNode;
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
    public Double getStrength() {
        return Strength;
    }

    @Override
    public void setStrength(Double Strength) {
        this.Strength = Strength;
    }

    @Override
    public NetworkNode getReceivingNode() {
        return receivingNode;
    }

    @Override
    public void setReceivingNode(NetworkNode receivingNode) {
        this.receivingNode = (NetworkNodeEntity) receivingNode;
    }

    @Override
    public NetworkNode getSendingNode() {
        return sendingNode;
    }

    @Override
    public void setSendingNode(NetworkNode sendingNode) {
        this.sendingNode = (NetworkNodeEntity) sendingNode;
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
        if (!(object instanceof NetworkConnectionEntity)) {
            return false;
        }
        NetworkConnectionEntity other = (NetworkConnectionEntity) object;
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
