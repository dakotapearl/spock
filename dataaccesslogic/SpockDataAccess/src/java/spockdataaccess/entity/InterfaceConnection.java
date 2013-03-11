package spockdataaccess.entity;

import java.io.Serializable;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name="InterfaceConnections")
public class InterfaceConnection implements Serializable {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @ManyToOne
    protected NetworkInterface networkInterface;
    @ManyToOne
    protected EnvironmentInterface environmentInterface;
    @ManyToOne
    protected Experiment experiment;

    public InterfaceConnection() {
    }

    public InterfaceConnection(NetworkInterface networkInterface, EnvironmentInterface environmentInterface, Experiment experiment) {
        this.networkInterface = networkInterface;
        this.environmentInterface = environmentInterface;
        this.experiment = experiment;
    }
    
    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public EnvironmentInterface getEnvironmentInterface() {
        return environmentInterface;
    }

    public void setEnvironmentInterface(EnvironmentInterface environmentInterface) {
        this.environmentInterface = environmentInterface;
    }

    public Experiment getExperiment() {
        return experiment;
    }

    public void setExperiment(Experiment experiment) {
        this.experiment = experiment;
    }

    public NetworkInterface getNetworkInterface() {
        return networkInterface;
    }

    public void setNetworkInterface(NetworkInterface networkInterface) {
        this.networkInterface = networkInterface;
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
        if (!(object instanceof InterfaceConnection)) {
            return false;
        }
        InterfaceConnection other = (InterfaceConnection) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.InterfaceConnection[ id=" + id + " ]";
    }
    
}
