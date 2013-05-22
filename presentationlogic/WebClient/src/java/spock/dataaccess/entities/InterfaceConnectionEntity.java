package spock.dataaccess.entities;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import spock.dataaccess.ejb.interfaces.entities.EnvironmentInterface;
import spock.dataaccess.ejb.interfaces.entities.Experiment;
import spock.dataaccess.ejb.interfaces.entities.InterfaceConnection;
import spock.dataaccess.ejb.interfaces.entities.NetworkInterface;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name="InterfaceConnections")
public class InterfaceConnectionEntity implements InterfaceConnection {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @ManyToOne
    protected NetworkInterfaceEntity networkInterface;
    @ManyToOne
    protected EnvironmentInterfaceEntity environmentInterface;
    @ManyToOne
    protected ExperimentEntity experiment;

    public InterfaceConnectionEntity() {
    }

    public InterfaceConnectionEntity(NetworkInterface networkInterface, EnvironmentInterface environmentInterface, Experiment experiment) {
        this.networkInterface = (NetworkInterfaceEntity) networkInterface;
        this.environmentInterface = (EnvironmentInterfaceEntity) environmentInterface;
        this.experiment = (ExperimentEntity) experiment;
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
    public EnvironmentInterface getEnvironmentInterface() {
        return environmentInterface;
    }

    @Override
    public void setEnvironmentInterface(EnvironmentInterface environmentInterface) {
        this.environmentInterface = (EnvironmentInterfaceEntity) environmentInterface;
    }

    @Override
    public Experiment getExperiment() {
        return experiment;
    }

    @Override
    public void setExperiment(Experiment experiment) {
        this.experiment = (ExperimentEntity) experiment;
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
        if (!(object instanceof InterfaceConnectionEntity)) {
            return false;
        }
        InterfaceConnectionEntity other = (InterfaceConnectionEntity) object;
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
