package spockdataaccess.entity;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name = "Networks")
public class Network implements Serializable {
    private static final long serialVersionUID = 1L;
    
    @Id
    private String id;
    @NotNull
    protected Boolean isActive;
    @ManyToMany(mappedBy="networks")
    protected Collection<Experiment> experiments;
    @OneToMany(mappedBy="network")
    protected Collection<NetworkNode> networkNodes;
    @OneToMany(mappedBy="network")
    protected Collection<NetworkInterface> networkInterfaces;
    @ManyToMany
    protected Collection<Metric> metrics;
    
    public Network() {
        experiments = new ArrayList<Experiment>();
        networkNodes = new ArrayList<NetworkNode>();
        networkInterfaces = new ArrayList<NetworkInterface>();
        metrics = new ArrayList<Metric>();
    }

    public Network(String id, Boolean isActive) {
        experiments = new ArrayList<Experiment>();
        networkNodes = new ArrayList<NetworkNode>();
        networkInterfaces = new ArrayList<NetworkInterface>();
        metrics = new ArrayList<Metric>();
        this.id = id;
        this.isActive = isActive;
    }
    
    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public Boolean getIsActive() {
        return isActive;
    }

    public void setIsActive(Boolean isActive) {
        this.isActive = isActive;
    }

    public Collection<Experiment> getExperiments() {
        return experiments;
    }
    
    public void addExperiment(Experiment experiment) {
        experiments.add(experiment);
    }

    public Collection<NetworkNode> getNodes() {
        return networkNodes;
    }
    
    public Collection<Metric> getMetrics() {
        return metrics;
    }

    public void setMetrics(Collection<Metric> metrics) {
        this.metrics = metrics;
    }

    public Collection<NetworkInterface> getNetworkInterfaces() {
        return networkInterfaces;
    }

    public void setNetworkInterfaces(Collection<NetworkInterface> networkInterfaces) {
        this.networkInterfaces = networkInterfaces;
    }

    public Collection<NetworkNode> getNetworkNodes() {
        return networkNodes;
    }

    public void setNetworkNodes(Collection<NetworkNode> networkNodes) {
        this.networkNodes = networkNodes;
    }
    
    public void addNetworkInterface(NetworkInterface networkInterface) {
        networkInterfaces.add(networkInterface);
    }
    
    public void addMetric(Metric metric) {
        metrics.add(metric);
    }
    
    public void addNetworkNode(NetworkNode networkNode) {
        networkNodes.add(networkNode);
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
        if (!(object instanceof Network)) {
            return false;
        }
        Network other = (Network) object;
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
