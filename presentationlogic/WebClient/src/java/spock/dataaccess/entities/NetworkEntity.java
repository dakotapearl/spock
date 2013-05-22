package spock.dataaccess.entities;

import java.util.ArrayList;
import java.util.Collection;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import spock.dataaccess.ejb.interfaces.entities.Experiment;
import spock.dataaccess.ejb.interfaces.entities.Metric;
import spock.dataaccess.ejb.interfaces.entities.Network;
import spock.dataaccess.ejb.interfaces.entities.NetworkInterface;
import spock.dataaccess.ejb.interfaces.entities.NetworkNode;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name = "Networks")
public class NetworkEntity implements Network {
    private static final long serialVersionUID = 1L;
    
    @Id
    private String id;
    @NotNull
    protected Boolean isActive;
    @ManyToMany(mappedBy="networks")
    protected Collection<ExperimentEntity> experiments;
    @OneToMany(mappedBy="network")
    protected Collection<NetworkNodeEntity> networkNodes;
    @OneToMany(mappedBy="network")
    protected Collection<NetworkInterfaceEntity> networkInterfaces;
    @ManyToMany
    protected Collection<MetricEntity> metrics;
    
    public NetworkEntity() {
        experiments = new ArrayList<ExperimentEntity>();
        networkNodes = new ArrayList<NetworkNodeEntity>();
        networkInterfaces = new ArrayList<NetworkInterfaceEntity>();
        metrics = new ArrayList<MetricEntity>();
    }

    public NetworkEntity(String id, Boolean isActive) {
        experiments = new ArrayList<ExperimentEntity>();
        networkNodes = new ArrayList<NetworkNodeEntity>();
        networkInterfaces = new ArrayList<NetworkInterfaceEntity>();
        metrics = new ArrayList<MetricEntity>();
        this.id = id;
        this.isActive = isActive;
    }
    
    @Override
    public String getId() {
        return id;
    }

    @Override
    public void setId(String id) {
        this.id = id;
    }

    @Override
    public Boolean getIsActive() {
        return isActive;
    }

    @Override
    public void setIsActive(Boolean isActive) {
        this.isActive = isActive;
    }

    @Override
    public Collection<Experiment> getExperiments() {
        return (Collection) experiments;
    }
    
    @Override
    public void addExperiment(Experiment experiment) {
        experiments.add((ExperimentEntity) experiment);
    }

    @Override
    public Collection<NetworkNode> getNodes() {
        return (Collection) networkNodes;
    }
    
    @Override
    public Collection<Metric> getMetrics() {
        return (Collection) metrics;
    }

    @Override
    public void setMetrics(Collection<Metric> metrics) {
        this.metrics = (Collection) metrics;
    }

    @Override
    public Collection<NetworkInterface> getNetworkInterfaces() {
        return (Collection) networkInterfaces;
    }

    @Override
    public void setNetworkInterfaces(Collection<NetworkInterface> networkInterfaces) {
        this.networkInterfaces = (Collection) networkInterfaces;
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
    public void addNetworkInterface(NetworkInterface networkInterface) {
        networkInterfaces.add((NetworkInterfaceEntity) networkInterface);
    }
    
    @Override
    public void addMetric(Metric metric) {
        metrics.add((MetricEntity) metric);
    }
    
    @Override
    public void addNetworkNode(NetworkNode networkNode) {
        networkNodes.add((NetworkNodeEntity) networkNode);
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
        if (!(object instanceof NetworkEntity)) {
            return false;
        }
        NetworkEntity other = (NetworkEntity) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.Network[ id=" + id + " ]";
    }
    
}
