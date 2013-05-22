package spock.dataaccess.entities;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import spock.dataaccess.ejb.interfaces.entities.Environment;
import spock.dataaccess.ejb.interfaces.entities.EnvironmentInterface;
import spock.dataaccess.ejb.interfaces.entities.EnvironmentNode;
import spock.dataaccess.ejb.interfaces.entities.Experiment;
import spock.dataaccess.ejb.interfaces.entities.Metric;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name = "Environments")
public class EnvironmentEntity implements Environment {
    private static final long serialVersionUID = 1L;
    @Id
    private String id;
    @NotNull
    protected Boolean isActive;
    @ManyToMany(mappedBy="environments")
    protected Collection<ExperimentEntity> experiments;
    @NotNull
    protected String CodeURL;
    @NotNull
    protected String DataURL;
    @OneToMany(mappedBy="environment")
    protected Collection<EnvironmentNodeEntity> environmentNodes;
    @OneToMany(mappedBy="environment")
    protected Collection<EnvironmentInterfaceEntity> environmentInterfaces;
    @ManyToMany
    protected Collection<MetricEntity> metrics;
    
    public EnvironmentEntity() {
        experiments = new ArrayList<ExperimentEntity>();
        environmentNodes = new ArrayList<EnvironmentNodeEntity>();
        environmentInterfaces = new ArrayList<EnvironmentInterfaceEntity>();
        metrics = new ArrayList<MetricEntity>();
    }

    public EnvironmentEntity(String id, Boolean isActive, String CodeURL, String DataURL) {
        experiments = new ArrayList<ExperimentEntity>();
        environmentNodes = new ArrayList<EnvironmentNodeEntity>();
        environmentInterfaces = new ArrayList<EnvironmentInterfaceEntity>();
        metrics = new ArrayList<MetricEntity>();
        this.id = id;
        this.isActive = isActive;
        this.CodeURL = CodeURL;
        this.DataURL = DataURL;
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
    public String getCodeURL() {
        return CodeURL;
    }

    @Override
    public void setCodeURL(String CodeURL) {
        this.CodeURL = CodeURL;
    }

    @Override
    public String getDataURL() {
        return DataURL;
    }

    @Override
    public void setDataURL(String DataURL) {
        this.DataURL = DataURL;
    }

    @Override
    public Collection<EnvironmentInterface> getEnvironmentInterfaces() {
        return (List) environmentInterfaces;
    }

    @Override
    public void setEnvironmentInterfaces(Collection<EnvironmentInterface> environmentInterfaces) {
        this.environmentInterfaces = (List) environmentInterfaces;
    }

    @Override
    public Collection<EnvironmentNode> getEnvironmentNodes() {
        return (List) environmentNodes;
    }

    @Override
    public void setEnvironmentNodes(Collection<EnvironmentNode> environmentNodes) {
        this.environmentNodes = (List) environmentNodes;
    }
    
    @Override
    public void addEnvironmentInterface(EnvironmentInterface environmentInterface) {
        environmentInterfaces.add((EnvironmentInterfaceEntity) environmentInterface);
    }
    
    @Override
    public void addEnvironmentNode(EnvironmentNode environmentNode) {
        environmentNodes.add((EnvironmentNodeEntity) environmentNode);
    }

    @Override
    public Collection<Metric> getMetrics() {
        return (List) metrics; //TODO Find better soln! This sacrifices all type safety
    }

    @Override
    public void setMetrics(Collection<Metric> metrics) {
        this.metrics = (List) metrics; //TODO Find better soln! This sacrifices all type safety
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
        if (!(object instanceof EnvironmentEntity)) {
            return false;
        }
        EnvironmentEntity other = (EnvironmentEntity) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.Environment[ id=" + id + " ]";
    }
    
}
