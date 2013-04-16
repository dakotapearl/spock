package spock.dataaccess.entities;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import spock.dataaccess.ejb.interfaces.entities.Environment;
import spock.dataaccess.ejb.interfaces.entities.EnvironmentInterface;
import spock.dataaccess.ejb.interfaces.entities.EnvironmentNode;

/**
 * This entity can only exist with an environment and an environment interface.
 * 
 * @author Loren Chorley
 */
@Entity
@Table(name = "EnvironmentNodes")
public class EnvironmentNodeEntity implements EnvironmentNode {
    private static final long serialVersionUID = 1L;
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private Long id;
    @NotNull
    @ManyToOne
    protected EnvironmentEntity environment;
    @NotNull
    @ManyToOne
    protected EnvironmentInterfaceEntity environmentInterface;

    public EnvironmentNodeEntity() {
    }

    public EnvironmentNodeEntity(Environment environment, EnvironmentInterface environmentInterface) {
        this.environment = (EnvironmentEntity) environment;
        this.environmentInterface = (EnvironmentInterfaceEntity) environmentInterface;
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
    public Environment getEnvironment() {
        return environment;
    }

    @Override
    public EnvironmentInterface getEnvironmentInterface() {
        return environmentInterface;
    }

    @Override
    public void setEnvironment(Environment environment) {
        this.environment = (EnvironmentEntity) environment;
    }

    @Override
    public void setEnvironmentInterface(EnvironmentInterface environmentInterface) {
        this.environmentInterface = (EnvironmentInterfaceEntity) environmentInterface;
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
        if (!(object instanceof EnvironmentNodeEntity)) {
            return false;
        }
        EnvironmentNodeEntity other = (EnvironmentNodeEntity) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.EnvironmentNode[ id=" + id + " ]";
    }
    
}
