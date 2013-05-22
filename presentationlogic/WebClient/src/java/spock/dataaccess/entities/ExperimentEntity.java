package spock.dataaccess.entities;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import javax.ejb.EJBException;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.ManyToMany;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import spock.dataaccess.ejb.interfaces.entities.Environment;
import spock.dataaccess.ejb.interfaces.entities.Experiment;
import spock.dataaccess.ejb.interfaces.entities.Network;
import spock.dataaccess.ejb.interfaces.entities.UserInterface;

/**
 *
 * @author Loren Chorley
 */
@Entity
@Table(name = "Experiments")
public class ExperimentEntity implements Experiment {
    private static final long serialVersionUID = 1L;
    @Id
    private String id;
    @NotNull
    protected Boolean isActive;
    @ManyToMany
    protected Collection<NetworkEntity> networks;
    @ManyToMany
    protected Collection<EnvironmentEntity> environments;
    @OneToMany(mappedBy="experiment")
    protected Collection<UserInterfaceEntity> interfaces;
    
    public ExperimentEntity() {
        networks = new ArrayList<NetworkEntity>();
        environments = new ArrayList<EnvironmentEntity>();
        interfaces = new ArrayList<UserInterfaceEntity>();
    }

    public ExperimentEntity(String id, Boolean isActive) {
        networks = new ArrayList<NetworkEntity>();
        environments = new ArrayList<EnvironmentEntity>();
        interfaces = new ArrayList<UserInterfaceEntity>();
        this.id = id;
        this.isActive = isActive;
    }
    
    public ExperimentEntity(String id, boolean isActive) {
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
    public void setIsActive(Boolean IsActive) {
        this.isActive = IsActive;
    }

    @Override
    public Collection<Network> getNetworks() {
        return (Collection) networks;
    }
    
    @Override
    public void addNetwork(Network network) {
        if (networks.contains((NetworkEntity) network)) {
            throw new EJBException("Experiment threw: Already contains network " + network.getId());
        }
        networks.add((NetworkEntity) network);
    }
    
    @Override
    public void removeNetwork(Network network) {
        if (!networks.contains((NetworkEntity) network)) {
            throw new EJBException("Experiment threw: Does not contain network " + network.getId());
        }
        networks.remove((NetworkEntity) network);
    }

    @Override
    public Collection<Environment> getEnvironments() {
        return (Collection) environments;
    }
    
    @Override
    public void addEnvironment(Environment environment) {
        if (environments.contains((EnvironmentEntity) environment)) {
            throw new EJBException("Experiment threw: Already contains environment " + environment.getId());
        }
        environments.add((EnvironmentEntity) environment);
    }
    
    @Override
    public void removeEnvironment(Environment environment) {
        if (!environments.contains((EnvironmentEntity) environment)) {
            throw new EJBException("Experiment threw: Does not contain environment " + environment.getId());
        }
        environments.remove((EnvironmentEntity) environment);
    }

    @Override
    public Collection<UserInterface> getInterfaces() {
        return (Collection) interfaces;
    }
    
    @Override
    public void addUserInterface(UserInterface userInterface) {
        interfaces.add((UserInterfaceEntity) userInterface);
    }
    
    @Override
    public void removeUserInterface(UserInterface userInterface) {
        interfaces.remove((UserInterfaceEntity) userInterface);
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
        if (!(object instanceof ExperimentEntity)) {
            return false;
        }
        ExperimentEntity other = (ExperimentEntity) object;
        if ((this.id == null && other.id != null) || (this.id != null && !this.id.equals(other.id))) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "spockdataaccess.entity.Experiment[ id=" + id + " ]";
    }
    
}
