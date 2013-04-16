package spock.dataaccess.entities;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import spock.dataaccess.ejb.interfaces.entities.Environment;
import spock.dataaccess.ejb.interfaces.entities.EnvironmentInterface;
import spock.dataaccess.ejb.interfaces.entities.EnvironmentNode;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name = "EnvironmentInterfaces")
public class EnvironmentInterfaceEntity implements EnvironmentInterface {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    protected Integer NumberOfNodes;
    @OneToMany(mappedBy="environmentInterface")
    protected Collection<EnvironmentNodeEntity> environmentNodes;
    @NotNull
    @ManyToOne
    protected EnvironmentEntity environment;
    @NotNull
    protected Boolean isInputInterface;
    
    public EnvironmentInterfaceEntity() {
        environmentNodes = new ArrayList<EnvironmentNodeEntity>();
    }

    public EnvironmentInterfaceEntity(Integer NumberOfNodes, Environment environment, Boolean isInputInterface) {
        environmentNodes = new ArrayList<EnvironmentNodeEntity>();
        this.NumberOfNodes = NumberOfNodes;
        this.environment = (EnvironmentEntity) environment;
        this.isInputInterface = isInputInterface;
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
    public Collection<EnvironmentNode> getEnvironmentNodes() {
        return (Collection) environmentNodes;
    }

    @Override
    public void setEnvironmentNodes(Collection<EnvironmentNode> environmentNodes) {
        this.environmentNodes = (Collection) environmentNodes;
    }

    @Override
    public void addEnvironmentNode(EnvironmentNode environmentNode) {
        environmentNodes.add((EnvironmentNodeEntity) environmentNode);
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
    public Environment getEnvironment() {
        return environment;
    }

    @Override
    public void setEnvironment(Environment environment) {
        this.environment = (EnvironmentEntity) environment;
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
        if (!(object instanceof EnvironmentInterfaceEntity)) {
            return false;
        }
        EnvironmentInterfaceEntity other = (EnvironmentInterfaceEntity) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.EnvironmentInterface[ id=" + id + " ]";
    }
    
}
